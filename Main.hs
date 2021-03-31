{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lex as L

import Data.Foldable
import qualified Data.ByteString.Lazy.Char8 as B

test :: B.ByteString -> IO ()
test s = for_ (L.lex s) print

main :: IO ()
main = do
  test "1 hello world"
  test "2 hello (* world *)"
  test "3 (* hello (* world *) *)"
  test "4 (* hello ((* world *) *)"
  test "5 (* hello (* world *) **)"
