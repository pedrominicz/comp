{-# LANGUAGE OverloadedStrings #-}

module Test where

import Lex as L
import Parse
import Syntax as S

import Data.List
import Data.Maybe

lexTests :: [Bool]
lexTests =
  [ scan "/**///*/*/*" == Nothing
  ]

parseTests :: [Bool]
parseTests =
  -- Expressions
  [ (parseExpr =<< scan "42")          == Just (S.IntLiteral 42)
  , (parseExpr =<< scan "f()")         == Just (S.Call (S.Identifier "f") [])
  , (parseExpr =<< scan "f(42)")       == Just (S.Call (S.Identifier "f") [S.IntLiteral 42])
  , (parseExpr =<< scan "(f + 2)(42)") == Just (S.Call (S.Add (S.Identifier "f") (S.IntLiteral 2)) [S.IntLiteral 42])
  , (parseExpr =<< scan "f(4, 2)")     == Just (S.Call (S.Identifier "f") [S.IntLiteral 4, S.IntLiteral 2])
  , (parseExpr =<< scan "x = y + 2")   == Just (S.Assignment "x" (S.Add (S.Identifier "y") (S.IntLiteral 2)))
  , (parseExpr =<< scan "x + y + z")   == Just (S.Add (S.Add (S.Identifier "x") (S.Identifier "y")) (S.Identifier "z"))
  , (parseExpr =<< scan "x = y = z")   == Just (S.Assignment "x" (S.Assignment "y" (S.Identifier "z")))
  -- Statements
  , (parseStmt =<< scan "42;")                      == Just (S.Expr (S.IntLiteral 42))
  , (parseStmt =<< scan "return 42;")               == Just (S.Return (S.IntLiteral 42))
  , (parseStmt =<< scan "if(1) { }")                == Nothing
  , (parseStmt =<< scan "if(1) { 2; }")             == Just (S.If (S.IntLiteral 1) [(S.Expr (S.IntLiteral 2))])
  , (parseStmt =<< scan "if(0) { 1; 2; 3; }")       == Just (S.If (S.IntLiteral 0) [(S.Expr (S.IntLiteral 1)), (S.Expr (S.IntLiteral 2)), (S.Expr (S.IntLiteral 3))])
  , (parseStmt =<< scan "if(0) { 1; } else { 2; }") == Just (S.IfElse (S.IntLiteral 0) [(S.Expr (S.IntLiteral 1))] [(S.Expr (S.IntLiteral 2))])
  , (parseStmt =<< scan "while(x) { x = x + 1; }")  == Just (S.While (S.Identifier "x") [S.Expr (S.Assignment "x" (S.Add (S.Identifier "x") (S.IntLiteral 1)))])
  -- Functions
  , (parseFunc =<< scan "f() { return 0; }")               == Just (S.Func "f" [] [] [S.Return (S.IntLiteral 0)])
  , (parseFunc =<< scan "f() { x = 0; return x; }")        == Just (S.Func "f" [] [] [S.Expr (S.Assignment "x" (S.IntLiteral 0)), S.Return (S.Identifier "x")])
  , (parseFunc =<< scan "f() { int x; return 0; }")        == Just (S.Func "f" [] ["x"] [S.Return (S.IntLiteral 0)])
  , (parseFunc =<< scan "f() { int x; int y; return 0; }") == Just (S.Func "f" [] ["x", "y"] [S.Return (S.IntLiteral 0)])
  , (parseFunc =<< scan "f(int x) { return 0; }")          == Just (S.Func "f" ["x"] [] [S.Return (S.IntLiteral 0)])
  , (parseFunc =<< scan "f(int x, int y) { return 0; }")   == Just (S.Func "f" ["x", "y"] [] [S.Return (S.IntLiteral 0)])
  ]

tests :: [Bool]
tests = lexTests ++ parseTests

main :: IO ()
main =
  case findIndex not tests of
    Just i -> putStrLn $ unwords ["Test", show i, "failed."]
    Nothing -> putStrLn "All tests were successful."
