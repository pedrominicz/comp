{
module Lex (Lexeme, Lex.lex) where

import qualified Data.ByteString.Lazy as B
}

%wrapper "monad-bytestring"

$digit = [0-9]
$lower = [a-z]
$upper = [A-Z]

tokens :-
  -- Comments
  "(*"          { comment }

  -- Identifiers
  ($digit | $lower | $upper)+ { identifier }

  -- Whitespace
  $white+       ;

{
type Lexeme = (AlexPosn, B.ByteString)

alexEOF :: Alex Lexeme
alexEOF = return (undefined, B.empty)

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

identifier :: AlexInput -> Int64 -> Alex Lexeme
identifier (pos, _, str, _) len = return (pos, B.take len str)

lex :: B.ByteString -> [Lexeme]
lex str = done $ runAlex str go
  where
  go = do
    (pos, lexeme) <- alexMonadScan
    if B.null lexeme
      then return []
      else go >>= return . ((pos, lexeme) :)

  done (Left msg) = error msg
  done (Right lexemes) = lexemes
}
