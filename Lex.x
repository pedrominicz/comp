{
module Lex (Token(..), scan) where

import Data.Either.Combinators
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.ByteString.Lazy.Char8 as ByteString (readInt)
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
  $digit+       { intLiteral }
  \" (. # [\"\\] | \\ .)* \" { stringLiteral }

  -- Operators
  "<<"          { mk ShiftLeft }
  ">>"          { mk ShiftRight }
  "<="          { mk LE }
  ">="          { mk GE }
  "=="          { mk EQ' } -- Avoids conflict with `Prelude.EQ`.
  "!="          { mk NE }
  ";"           { mk Semicolon }
  "{"           { mk OpenBrace }
  "}"           { mk CloseBrace }
  ","           { mk Comma }
  "="           { mk Equals }
  "("           { mk OpenParen }
  ")"           { mk CloseParen }
  "&"           { mk BitwiseAnd }
  "!"           { mk Not }
  "~"           { mk BitwiseNot }
  "-"           { mk Minus }
  "+"           { mk Plus }
  "*"           { mk Asterisk }
  "/"           { mk Div }
  "%"           { mk Mod }
  "<"           { mk LT' } -- Avoids conflict with `Prelude.LT`.
  ">"           { mk GT' } -- Avoids conflict with `Prelude.GT`.
  "^"           { mk BitwiseXor }
  "|"           { mk BitwiseOr }

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
  | Identifier ByteString
  -- Literals
  | IntLiteral Int
  | StringLiteral ByteString
  -- Operators
  | ShiftLeft   -- <<
  | ShiftRight  -- >>
  | LE          -- <=
  | GE          -- >=
  | EQ'         -- == (avoids conflict with `Prelude.EQ`)
  | NE          -- !=
  | Semicolon   -- ;
  | OpenBrace   -- {
  | CloseBrace  -- }
  | Comma       -- ,
  | Equals      -- =
  | OpenParen   -- (
  | CloseParen  -- )
  | BitwiseAnd  -- &
  | Not         -- !
  | BitwiseNot  -- ~
  | Minus       -- -
  | Plus        -- +
  | Asterisk    -- *
  | Div         -- /
  | Mod         -- %
  | LT'         -- < (avoids conflict with `Prelude.LT`)
  | GT'         -- > (avoids conflict with `Prelude.GT`)
  | BitwiseXor  -- ^
  | BitwiseOr   -- |
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
identifier (_, _, str, _) len = return . Identifier $ ByteString.take len str

intLiteral :: AlexInput -> Int64 -> Alex Token
intLiteral (_, _, str, _) len =
  return . IntLiteral . readInt $ ByteString.take len str

stringLiteral :: AlexInput -> Int64 -> Alex Token
stringLiteral (_, _, str, _) len =
  return . StringLiteral $ ByteString.take len str

readInt :: ByteString -> Int
readInt str =
  case ByteString.readInt str of
    Just (x, empty) -> x
    _ -> error "unreachable"

scan :: ByteString -> Maybe [Token]
scan str = rightToMaybe $ runAlex str go
  where
  go = do
    token <- alexMonadScan
    case token of
      EOF -> return []
      token -> go >>= return . (token :)
}
