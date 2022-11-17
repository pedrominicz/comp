{
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lex (Token(..), lexer) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
}

%wrapper "strict-bytestring"

$alpha = [A-Za-z]
$digit = [0-9]

@var = [$alpha] [$alpha $digit _]*

tokens :-
  $white+       ;
  @var          { Var }
  "\" | "λ"     { const Lam }
  ","           { const Comma }
  "("           { const LParen }
  ")"           { const RParen }
  "--" [^\n\r]* ;

{
data Token
  = Var ByteString
  | Lam
  | Comma
  | LParen
  | RParen

lexer :: ByteString -> Maybe [Token]
lexer str = go (AlexInput '\n' str 0)
  where
  go :: AlexInput -> Maybe [Token]
  go input@(AlexInput _ str _) =
    case alexScan input 0 of
      AlexEOF -> Just []
      AlexError _ -> Nothing
      AlexSkip input _ -> go input
      AlexToken input len action -> (action (B.take len str) :) <$> go input
}
