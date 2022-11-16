module Main where

import Eval
import Parse
import Pretty

import Data.Maybe

test :: String -> IO ()
test = putStrLn . pretty . eval . fromJust . parse

main :: IO ()
main = do
  -- Basic
  test "(\\x,x)(\\x,x)"
  test "(\\x,x)(\\x,x)(\\x,x)"
  test "(\\x,x)((\\x,x)(\\x,x))"

  -- Numbers
  test "0"
  test "1"
  test "2"
  test "10"

  -- S, K
  test "\\x,\\y,\\z,x z(y z)"
  test "\\x,\\y,x"
  test "(\\x,\\y,\\z,x z(y z))(\\x,\\y,x)(\\x,\\y,x)"

  -- Factorial
  test "(\\n,\\f,n(\\c,\\i,i(c(\\f,\\x,i f(f x))))(\\x,f)(\\x,x))2"
  test "(\\n,\\f,n(\\c,\\i,i(c(\\f,\\x,i f(f x))))(\\x,f)(\\x,x))3"
  test "(\\n,\\f,n(\\c,\\i,i(c(\\f,\\x,i f(f x))))(\\x,f)(\\x,x))4"
  test "(\\n,\\f,n(\\c,\\i,i(c(\\f,\\x,i f(f x))))(\\x,f)(\\x,x))5"
