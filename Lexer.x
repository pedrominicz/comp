{
module Lexer (Lexeme, LexemeType(..), Lexer.lex) where

import Data.Either.Combinators
import qualified Data.ByteString.Lazy.Char8 as B
}

%wrapper "monad-bytestring"

$digit = [0-9]
$lower = [a-z]
$upper = [A-Z]

tokens :-
  $white+       ;
  "(*"          { comment }
  "("           { mk LParen }
  ")"           { mk RParen }
  "true"        { mk (Bool True) }
  "false"       { mk (Bool False) }
  "not"         { mk Not }
  $digit+       { int }
  $digit+ "." $digit* { float }
  "-"           { mk Minus }
  "+"           { mk Plus }
  "-."          { mk MinusDot }
  "+."          { mk PlusDot }
  "*."          { mk AstDot}
  "/."          { mk SlashDot }
  "="           { mk Equal }
  "<>"          { mk LessGreater }
  "<="          { mk LessEqual }
  ">="          { mk GreaterEqual }
  "<"           { mk Less }
  ">"           { mk Greater }
  "if"          { mk If }
  "then"        { mk Then }
  "else"        { mk Else }
  "let"         { mk Let }
  "in"          { mk In }
  "rec"         { mk Rec }
  ","           { mk Comma }
  "_"           { mk Underscore }
  "Array.create" | "Array.make" { mk ArrayCreate }
  "."           { mk Dot }
  "<-"          { mk LessMinus }
  ";"           { mk Semicolon }
  $lower ($digit | $lower | $upper | "_")* { identifier }

{
-- `(offset, line, len, lexemeType)`
type Lexeme = (Int, Int, Int64, LexemeType)

data LexemeType
  = Bool Bool
  | Int Int
  | Float Float
  | Not
  | Minus
  | Plus
  | MinusDot
  | PlusDot
  | AstDot
  | SlashDot
  | Equal
  | LessGreater
  | LessEqual
  | GreaterEqual
  | Less
  | Greater
  | If
  | Then
  | Else
  | Ident B.ByteString 
  | Let
  | In
  | Rec
  | Underscore
  | Comma
  | ArrayCreate
  | Dot
  | LessMinus
  | Semicolon
  | LParen
  | RParen
  | EOF
  | Error
  deriving (Eq, Show)

alexEOF :: Alex Lexeme
alexEOF = do
  (AlexPn offset line _, _, _, _) <- alexGetInput
  return (offset, line, 0, EOF)

next :: Alex Lexeme
next = do
  input@(_, _, _, start) <- alexGetInput
  startCode <- alexGetStartCode
  case alexScan input startCode of
    AlexEOF -> alexEOF
    AlexError (AlexPn offset line _, _, _, _) ->
      return (offset, line, 0, Error)
    AlexSkip input _ -> do
      alexSetInput input
      next
    AlexToken input'@(_, _, _, end) _ action -> do
      let len = end - start
      alexSetInput input'
      action input len

-- https://github.com/simonmar/alex/blob/master/examples/haskell.x
comment :: AlexInput -> Int64 -> Alex Lexeme
comment _ _ = alexGetInput >>= go 1
  where
  go 0 input = do
    alexSetInput input
    next
  go n input =
    case alexGetByte input of
      -- '*'
      Just (42, input) ->
        case alexGetByte input of
          -- ')'
          Just (41, input) -> go (n - 1) input
          -- '*'
          Just (42, _) -> go n input
          Just (_, input) -> go n input
          Nothing -> alexEOF
      -- '('
      Just (40, input) ->
        case alexGetByte input of
          -- '*'
          Just (42, input) -> go (n + 1) input
          -- '('
          Just (40, _) -> go n input
          Just (_, input) -> go n input
          Nothing -> alexEOF
      Just (_, input) -> go n input
      Nothing -> alexEOF

mk :: LexemeType -> AlexInput -> Int64 -> Alex Lexeme
mk lexemeType (AlexPn offset line _, _, _, _) len =
  return (offset, line, len, lexemeType)

int :: AlexInput -> Int64 -> Alex Lexeme
int (AlexPn offset line _, _, str, _) len =
  return (offset, line, len, Int . readInt $ B.take len str)
  where
  readInt str =
    case B.readInt str of
      Just (x, empty) -> x
      _ -> error "unreachable"

float :: AlexInput -> Int64 -> Alex Lexeme
float (AlexPn offset line _, _, str, _) len =
  return (offset, line, len, Float . read $ B.unpack lexeme)
  where
  lexeme =
    if B.last (B.take len str) == '.'
      then B.take (len - 1) str
      else B.take len str

identifier :: AlexInput -> Int64 -> Alex Lexeme
identifier (AlexPn offset line _, _, str, _) len =
  return (offset, line, len, Ident (B.take len str))

lex :: B.ByteString -> Either (Int, Int) [Lexeme]
lex str = fromRight' . runAlex str $ go []
  where
  go lexemes = do
    lexeme@(offset, line, _, lexemeType) <- next
    case lexemeType of
      EOF -> return . Right $ reverse lexemes
      Error -> return $ Left (offset, line)
      _ -> go (lexeme : lexemes)
}
