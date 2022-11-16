module Main where

import Eval
import Expr
import Parse

import Data.Foldable
import Data.Maybe

parse' :: String -> Expr
parse' = fromJust . parse

test :: String -> String -> IO ()
test str1 str2 =
  if eval (parse' str1) == parse' str2
    then return ()
    else do
      putStrLn str1
      putStrLn str2

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [_] = undefined
pairs (x1:x2:xs) = (x1, x2) : pairs xs

main :: IO ()
main = do
  lines <- lines <$> getContents
  traverse_ (uncurry test) (pairs lines)
