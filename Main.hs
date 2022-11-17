module Main (main) where

import Eval
import Expr
import Parse

import Data.Foldable
import Data.Maybe

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

parse' :: ByteString -> Expr
parse' = fromJust . parse

test :: ByteString -> ByteString -> IO ()
test str1 str2 =
  if eval (parse' str1) == parse' str2
    then return ()
    else do
      B.putStrLn str1
      B.putStrLn str2

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [_] = undefined
pairs (x1:x2:xs) = (x1, x2) : pairs xs

main :: IO ()
main = do
  lines <- B.lines <$> B.getContents
  traverse_ (uncurry test) (pairs lines)
