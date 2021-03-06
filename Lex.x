{
module Lex (Token(..)) where

import Control.Monad.Except
import qualified Data.ByteString.Lazy.Char8 as ByteString (readInt)
import qualified Data.ByteString.Lazy as ByteString
}

%wrapper "basic-bytestring"

$digit = [0-9]
$alpha = [a-zA-Z_]

tokens :-
  -- Comments
  -- TODO

  -- Keywords
  "else"        { const Else }
  "if"          { const If }
  "int"         { const Int }
  "return"      { const Return }
  "while"       { const While }

  -- Identifiers
  $alpha ($digit | $alpha)+ { Identifier }

  -- Literals
  $digit+       { IntLiteral . readInt }
  \" (. # [\"\\] | \\ .)* \" { StringLiteral }

  -- Operators
  ">>"          { const ShiftRight }
  "<<"          { const ShiftLeft }
  "<="          { const LE }
  ">="          { const GE }
  "=="          { const EQ' } -- Avoids conflict with `Prelude.EQ`.
  "!="          { const NE }
  ";"           { const Semicolon }
  "{"           { const OpenBrace }
  "}"           { const CloseBrace }
  ","           { const Comma }
  "="           { const Equals }
  "("           { const OpenParen }
  ")"           { const CloseParen }
  "&"           { const BitwiseAnd }
  "!"           { const Not }
  "~"           { const BitwiseNot }
  "-"           { const Minus }
  "+"           { const Plus }
  "*"           { const Asterisk }
  "/"           { const Div }
  "%"           { const Mod }
  "<"           { const LT' } -- Avoids conflict with `Prelude.LT`.
  ">"           { const GT' } -- Avoids conflict with `Prelude.GT`.
  "^"           { const BitwiseXor }
  "|"           { const BitwiseOr }

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
  | Identifier ByteString.ByteString
  -- Literals
  | IntLiteral Int
  | StringLiteral ByteString.ByteString
  -- Operators
  | ShiftRight  -- >>
  | ShiftLeft   -- <<
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
  deriving (Show)

readInt :: ByteString.ByteString -> Int
readInt str =
  case ByteString.readInt str of
    Just (x, empty) -> x
    _ -> error "unreachable"

tokenize :: ByteString.ByteString -> Except AlexInput [Token]
tokenize str = go (AlexInput '\n' str 0)
  where
  go :: AlexInput -> Except AlexInput [Token]
  go input =
    case alexScan input 0 of
      AlexEOF -> return []
      AlexError err -> throwError err
      AlexSkip input _ -> go input
      AlexToken input' _ act -> do
        let len = alexBytePos input' - alexBytePos input
        rest <- go input'
        return $ act (ByteString.take len (alexStr input)) : rest
}
