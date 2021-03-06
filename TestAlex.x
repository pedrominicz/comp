{
module Test where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
}

%wrapper "monad-bytestring"

$alpha = [a-zA-Z]

words :-
  $white+       { skip }
  $alpha+       { word }

{
data Token
  = Word AlexPosn ByteString
  | EOF

alexEOF :: Alex Token
alexEOF = return EOF

word :: AlexInput -> Int64 -> Alex Token
word (pos, _, str, _) len = return $ Word pos (ByteString.take len str)

scanner :: ByteString.ByteString -> Either String [ByteString]
scanner str = runAlex str go
  where
  go = do
    token <- alexMonadScan
    case token of
      Word pos word -> go >>= return . (word :)
      EOF -> return []
}
