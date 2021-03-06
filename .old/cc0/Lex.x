{
module Lex (Token(..), scan) where

import Data.Either.Combinators
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B (readInt)
}

%wrapper "monad-bytestring"

$digit = [0-9]
$alpha = [a-zA-Z_]

tokens :-
  -- Comments
  "/*"          { comment }

  -- Keywords
  "else"        { mk Else }
  "if"          { mk If }
  "int"         { mk Int }
  "return"      { mk Return }
  "while"       { mk While }

  -- Identifiers
  $alpha ($digit | $alpha)* { identifier }

  -- Literals
  $digit+       { int }

  -- Operators
  ";"           { mk Semicolon }
  "{"           { mk OpenBrace }
  "}"           { mk CloseBrace }
  ","           { mk Comma }
  "="           { mk Equals }
  "("           { mk OpenParen }
  ")"           { mk CloseParen }
  "+"           { mk Plus }

  -- Whitespace
  $white+       ;

{
data Token
  -- Keywords
  = Else
  | If
  | Int
  | Return
  | While
  -- Identifiers
  | Id B.ByteString
  -- Literals
  | IntLiteral Int
  -- Operators
  | Semicolon   -- ;
  | OpenBrace   -- {
  | CloseBrace  -- }
  | Comma       -- ,
  | Equals      -- =
  | OpenParen   -- (
  | CloseParen  -- )
  | Plus        -- +
  | EOF
  deriving (Eq, Show)

alexEOF :: Alex Token
alexEOF = return EOF

comment :: AlexInput -> Int64 -> Alex Token
comment _ _ = alexGetInput >>= go
  where
  go input = do
    case alexGetByte input of
      Nothing -> done input
      Just (42, input) ->
        case alexGetByte input of
          Nothing          -> done input
          Just (47, input) -> done input
          Just (42, _)     -> go input
          Just (_, input)  -> go input
      Just (c, input) -> go input

  done input = do
    alexSetInput input
    alexMonadScan

mk :: Token -> AlexInput -> Int64 -> Alex Token
mk t _ _ = return t

identifier :: AlexInput -> Int64 -> Alex Token
identifier (_, _, str, _) len = return . Id $ B.take len str

int :: AlexInput -> Int64 -> Alex Token
int (_, _, str, _) len =
  return . IntLiteral . readInt $ B.take len str

readInt :: B.ByteString -> Int
readInt str =
  case B.readInt str of
    Just (x, empty) -> x
    _ -> error "unreachable"

scan :: B.ByteString -> Maybe [Token]
scan str = rightToMaybe $ runAlex str go
  where
  go = do
    token <- alexMonadScan
    case token of
      EOF -> return []
      token -> go >>= return . (token :)
}
