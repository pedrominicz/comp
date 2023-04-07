module Main (main) where

import ANF
import Eval
import Parse

import Data.Foldable

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

test :: ByteString -> IO ()
test str = print $ eval <$> (parse str >>= normalize)

main :: IO ()
main = do
  lines <- B.lines <$> B.getContents
  traverse_ test lines
