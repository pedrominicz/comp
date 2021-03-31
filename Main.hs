{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lex as L

import Data.Foldable
import qualified Data.ByteString.Lazy.Char8 as B

test :: B.ByteString -> IO ()
test s = for_ (L.lex s) print

main :: IO ()
main = do
  test "(* hello (* world *) *) () true false 1024"
  test "Array.create"
  test "Array.make"
  test "not - + -. +. *. /. = <> <= >= < > if then else let in rec ,"
  test ". <- ;"
  test "_"
  test "10 10. 10.24"
