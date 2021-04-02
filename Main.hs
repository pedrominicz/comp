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
  str <- getLine
  case L.lex (B.pack str) of
    Left pos -> return ()
    Right lexemes -> do
      exp <- readLn
      case (P.parse lexemes) of
        Left lexeme -> print lexeme
        Right result ->
          if result == exp
            then main
            else do
              putStrLn $ "expression: " ++ str
              putStrLn $ "expected: " ++ show exp
              putStrLn $ "got: " ++ show result
              undefined
