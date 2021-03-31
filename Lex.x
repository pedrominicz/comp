{
module Lex (Lexeme, LexemeType, Lex.lex) where

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
  $digit+ "." $digit* { float }
  $digit+       { int }
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
  deriving (Eq, Show)

alexEOF :: Alex Lexeme
alexEOF = do
  (AlexPn offset line _, _, _, _) <- alexGetInput
  return (offset, line, 0, EOF)

-- https://github.com/simonmar/alex/blob/master/examples/haskell.x
comment :: AlexInput -> Int64 -> Alex Lexeme
comment _ _ = alexGetInput >>= go 1
  where
  go 0 input = do
    alexSetInput input
    alexMonadScan
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
          Nothing -> fail input
      -- '('
      Just (40, input) ->
        case alexGetByte input of
          -- '*'
          Just (42, input) -> go (n + 1) input
          -- '('
          Just (40, _) -> go n input
          Just (_, input) -> go n input
          Nothing -> fail input
      Just (_, input) -> go n input
      Nothing -> fail input

  fail = alexError . show

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

lex :: B.ByteString -> [Lexeme]
lex str = done $ runAlex str go
  where
  go = do
    lexeme@(_, _, _, lexemeType) <- alexMonadScan
    if lexemeType == EOF
      then return []
      else go >>= return . (lexeme :)

  done (Left msg) = error msg
  done (Right lexemes) = lexemes
}
