{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lexer as L
import Parser as P
import Syntax as S

import System.IO

import Data.Either.Combinators
import Data.Foldable
import qualified Data.ByteString.Lazy.Char8 as B

main :: IO ()
main = do
  str <- B.pack <$> getLine
  case L.lex str of
    Left pos -> return ()
    Right lexemes -> do
      exp <- readLn
      print $ fromRight' (P.parse lexemes) == exp
      main
