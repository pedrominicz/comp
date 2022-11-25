module Main (main) where

import ANF
import Parse

import Control.Monad
import Data.Foldable
import System.Exit

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

test :: ByteString -> IO ()
test str =
  let e1 = parse str >>= normalize
      e2 = parse str >>= normalize' in
  unless (e1 == e2) $ do
    B.putStrLn str
    print e1
    print e2
    exitFailure

main :: IO ()
main = do
  lines <- B.lines <$> B.getContents
  traverse_ test lines
  putStrLn "success"
