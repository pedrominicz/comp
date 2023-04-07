{
module Lex (Token(..), lexer) where

import Data.Word

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
}

$alpha = [A-Za-z]
$digit = [0-9]

@num = $digit+
@var = $alpha [$alpha $digit _]*

tokens :-
  $white+       ;
  "="           { const Equal }
  ","           { const Comma }
  "("           { const LParen }
  ")"           { const RParen }
  "\" | "λ"     { const Lam }
  "let"         { const Let }
  "in"          { const In }
  "+"           { const Plus }
  @num          { number }
  @var          { Var }
  "--" [^\n\r]* ;

{
type Byte = Word8

type AlexInput = ByteString

alexGetByte :: AlexInput -> Maybe (Byte, AlexInput)
alexGetByte = B.uncons

data Token
  = Var {-# UNPACK #-} !ByteString
  | Num {-# UNPACK #-} !Int
  | Equal
  | Comma
  | LParen
  | RParen
  | Lam
  | Let
  | In
  | Plus
  deriving (Eq, Show)

number :: AlexInput -> Token
number = Num . B.foldl' (\x c -> x * 10 + fromEnum c - fromEnum '0') 0

lexer :: ByteString -> Maybe [Token]
lexer input =
  case alexScan input 0 of
    AlexEOF -> Just []
    AlexError _ -> Nothing
    AlexSkip input _ -> lexer input
    AlexToken input' len action ->
      (action (B.take len input) :) <$> lexer input'
}
