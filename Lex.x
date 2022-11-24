{
module Lex (Token(..), lexer) where

import Data.Word

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
}

$alpha = [A-Za-z]
$digit = [0-9]

@var = [$alpha] [$alpha $digit _]*

tokens :-
  $white+       ;
  "="           { const Equal }
  ","           { const Comma }
  "("           { const LParen }
  ")"           { const RParen }
  "\" | "λ"     { const Lam }
  "let"         { const Let }
  "in"          { const In }
  @var          { Var }
  "--" [^\n\r]* ;

{
type Byte = Word8

type AlexInput = ByteString

alexGetByte :: AlexInput -> Maybe (Byte, AlexInput)
alexGetByte = B.uncons

data Token
  = Var {-# UNPACK #-} !ByteString
  | Equal
  | Comma
  | LParen
  | RParen
  | Lam
  | Let
  | In

lexer :: ByteString -> Maybe [Token]
lexer input =
  case alexScan input 0 of
    AlexEOF -> Just []
    AlexError _ -> Nothing
    AlexSkip input _ -> lexer input
    AlexToken input' len action ->
      (action (B.take len input) :) <$> lexer input'
}
