{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lex as L
import Parse as P

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
  -- Should fail at "world".
  test "(* こんにちは世界 *) hello world"
  test "2 + 3 *. 4"
  test "2 + 3 + 4"
  test "(2 + 3) *. 4"
