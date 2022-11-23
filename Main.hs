module Main (main) where

import Eval
import Expr
import Parse

import qualified Eval.CEK as CEK
import qualified Eval.Krivine as Krivine
import qualified Eval.Strict as Strict

import Data.Foldable
import Data.Maybe
import System.Exit

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

parse' :: ByteString -> Expr
parse' = fromJust . parse

assertEq :: Eq a => a -> a -> IO ()
assertEq x1 x2 =
  if x1 == x2
    then return ()
    else exitFailure

test :: ByteString -> ByteString -> IO ()
test str1 str2 = do
  let e1 = parse' str1
      e2 = eval (parse' str2)
  assertEq e2 $ eval e1
  assertEq e2 $ eval (Krivine.eval e1)
  assertEq e2 $ eval (CEK.eval e1)
  assertEq e2 $ eval (Strict.eval e1)

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [_] = undefined
pairs (x1:x2:xs) = (x1, x2) : pairs xs

main :: IO ()
main = do
  lines <- B.lines <$> B.getContents
  traverse_ (uncurry test) (pairs lines)
  putStrLn "success"
