{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lexer as L
import Parser as P

import Data.Foldable
import qualified Data.ByteString.Lazy as B

test :: B.ByteString -> IO ()
test str =
  case L.lex str of
    Left pos -> print pos
    Right lexemes -> do
      let exp = P.parse lexemes
      print exp

main :: IO ()
main = do
  test "(* hello (* world *) *) ()"
  test "hello.(2)"
  test "10"
  test "10."
  test "10.24"
  test "(* (**) % (***) *) hello (* world"
  -- Test unexpected end-of-file error.
  test ""
  test "2 + 3 *. 4"
  test "2 + 3 + 4"
  test "(2 + 3) *. 4"
  putStrLn ""
  test "a = b"
  test "a <> b"
  test "a <= b"
  test "a >= b"
  test "a < b"
  test "a > b"
  test "1 = 1 = true"
  test "2. +. 2. = 2. *. 2."
  putStrLn ""
  test "-2."
  test "----0."
  test "----2"
  test "2 + -2"
  test "2 + -2."
  test "-2 + 2"
  test "-2. + 2"
  putStrLn ""
  test "hello world"
  test "a -b"
  test "-a b"
  test "f x y -z"
  test "-f x y -z"
  putStrLn ""
  test "a, b, c"
  test "2. +. 2. = 2. *. 2., -2. + 2, -f x y -z"
  putStrLn ""
  test "a.(1) <- 1"
  test "a.(1) <- 1 + 2"
  test "a.(1) <- 1 = 2"
  test "a.(1) <- 1, 2"
  test "a.(0).(1) <- 1"
  test "a.(a.(0) <- 1) <- 1"
  putStrLn ""
  test "1;2;3"
  test "a.(1; 0) <- 1, 2"
  test "a.(a.(0) <- 1) <- 1;2"
