{-# LANGUAGE OverloadedStrings #-}

module Test where

import Convert
import Instruction as I
import Lex as L
import Parse
import Syntax as S

import System.Exit

import Data.List
import Data.Maybe

lexTests :: [Bool]
lexTests =
  [ scan "int"         == Just [L.Int]
  , scan "42"          == Just [L.IntLiteral 42]
  , scan "/**///*/*/*" == Nothing
  ]

parseTests :: [Bool]
parseTests =
  -- Expressions
  [ (parseExpr =<< scan "42")          == Just (S.Int 42)
  , (parseExpr =<< scan "f()")         == Just (S.Call (S.Id "f") [])
  , (parseExpr =<< scan "f(42)")       == Just (S.Call (S.Id "f") [S.Int 42])
  , (parseExpr =<< scan "(f + 2)(42)") == Just (S.Call (S.Add (S.Id "f") (S.Int 2)) [S.Int 42])
  , (parseExpr =<< scan "f(4, 2)")     == Just (S.Call (S.Id "f") [S.Int 4, S.Int 2])
  , (parseExpr =<< scan "x = y + 2")   == Just (S.Assign "x" (S.Add (S.Id "y") (S.Int 2)))
  , (parseExpr =<< scan "x + y + z")   == Just (S.Add (S.Add (S.Id "x") (S.Id "y")) (S.Id "z"))
  , (parseExpr =<< scan "x + (y + z)") == Just (S.Add (S.Id "x") (S.Add (S.Id "y") (S.Id "z")))
  , (parseExpr =<< scan "x = y = z")   == Just (S.Assign "x" (S.Assign "y" (S.Id "z")))
  -- Statements
  , (parseStmt =<< scan "42;")                      == Just (S.Expr (S.Int 42))
  , (parseStmt =<< scan "return 42;")               == Just (S.Return (S.Int 42))
  , (parseStmt =<< scan "if(1) { }")                == Nothing
  , (parseStmt =<< scan "if(1) { 2; }")             == Just (S.If (S.Int 1) [(S.Expr (S.Int 2))])
  , (parseStmt =<< scan "if(0) { 1; 2; 3; }")       == Just (S.If (S.Int 0) [(S.Expr (S.Int 1)), (S.Expr (S.Int 2)), (S.Expr (S.Int 3))])
  , (parseStmt =<< scan "if(0) { 1; } else { 2; }") == Just (S.IfElse (S.Int 0) [(S.Expr (S.Int 1))] [(S.Expr (S.Int 2))])
  , (parseStmt =<< scan "while(x) { x = x + 1; }")  == Just (S.While (S.Id "x") [S.Expr (S.Assign "x" (S.Add (S.Id "x") (S.Int 1)))])
  -- Functions
  , (parseFunc =<< scan "f() { return 0; }")               == Just (S.Func "f" [] [] [S.Return (S.Int 0)])
  , (parseFunc =<< scan "f() { x = 0; return x; }")        == Just (S.Func "f" [] [] [S.Expr (S.Assign "x" (S.Int 0)), S.Return (S.Id "x")])
  , (parseFunc =<< scan "f() { int x; return 0; }")        == Just (S.Func "f" [] ["x"] [S.Return (S.Int 0)])
  , (parseFunc =<< scan "f() { int x; int y; return 0; }") == Just (S.Func "f" [] ["x", "y"] [S.Return (S.Int 0)])
  , (parseFunc =<< scan "f(int x) { return 0; }")          == Just (S.Func "f" ["x"] [] [S.Return (S.Int 0)])
  , (parseFunc =<< scan "f(int x, int y) { return 0; }")   == Just (S.Func "f" ["x", "y"] [] [S.Return (S.Int 0)])
  , (parseFunc =<< scan "f(int x) { int y; return 0; }")   == Just (S.Func "f" ["x"] ["y"] [S.Return (S.Int 0)])
  ]

convertTests :: [Bool]
convertTests =
  [ (convertExpr =<< parseExpr =<< scan "2 + 2 + 2")   == Just [I.Int 2, I.Int 2, I.Add, I.Int 2, I.Add]
  , (convertExpr =<< parseExpr =<< scan "2 + (2 + 2)") == Just [I.Int 2, I.Int 2, I.Int 2, I.Add, I.Add]
  ]

tests :: [Bool]
tests = lexTests ++ parseTests ++ convertTests

main :: IO ()
main =
  case findIndex not tests of
    Just i -> die $ unwords ["Test", show i, "failed."]
    Nothing -> putStrLn "All tests were successful."
