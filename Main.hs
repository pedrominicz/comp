{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lexer as L
import Parser as P

import System.IO

import Data.Maybe

main :: IO ()
main = do
  str <- getLine
  case L.lex str of
    Nothing -> do
      putChar '\n'
      return ()
    Just lexemes -> do
      let exp = fromJust $ P.parse lexemes
      exp' <- readLn
      if exp == exp'
        then do
          putChar '.'
          main
        else do
          putStrLn $ "expression: " ++ str
          putStrLn $ "expected: " ++ show exp'
          putStrLn $ "got: " ++ show exp
          undefined
