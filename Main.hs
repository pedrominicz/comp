module Main (main) where

import Eval
import Compile

import Data.Foldable

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

test :: ByteString -> IO ()
test str = print $ eval <$> compile str

main :: IO ()
main = do
  lines <- B.lines <$> B.getContents
  traverse_ test lines
