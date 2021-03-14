{-# LANGUAGE OverloadedStrings #-}

module Test where

import Convert
import Instruction as I
import Lex as L
import Parse
import Syntax as S

import System.Exit

import Control.Monad.RWS
import Data.List
import Data.Maybe
import Data.Tuple
import qualified Data.Map as M
import qualified Data.Set as S

lexTests :: [Bool]
lexTests =
  [ scan "int"         == Just [L.Int]
  , scan "42"          == Just [L.IntLiteral 42]
  , scan "/**///*/*/*" == Nothing
  ]

parseTests :: [Bool]
parseTests =
  -- Expressions
  [ go parseExpr "42"          == Just (S.Int 42)
  , go parseExpr "f()"         == Just (S.Call (S.Id "f") [])
  , go parseExpr "f(42)"       == Just (S.Call (S.Id "f") [S.Int 42])
  , go parseExpr "(f + 2)(42)" == Just (S.Call (S.Add (S.Id "f") (S.Int 2)) [S.Int 42])
  , go parseExpr "f(4, 2)"     == Just (S.Call (S.Id "f") [S.Int 4, S.Int 2])
  , go parseExpr "x = y + 2"   == Just (S.Assign "x" (S.Add (S.Id "y") (S.Int 2)))
  , go parseExpr "x + y + z"   == Just (S.Add (S.Add (S.Id "x") (S.Id "y")) (S.Id "z"))
  , go parseExpr "x + (y + z)" == Just (S.Add (S.Id "x") (S.Add (S.Id "y") (S.Id "z")))
  , go parseExpr "x = y = z"   == Just (S.Assign "x" (S.Assign "y" (S.Id "z")))
  -- Statements
  , go parseStmt "42;"                      == Just (S.Expr (S.Int 42))
  , go parseStmt "return 42;"               == Just (S.Return (S.Int 42))
  , go parseStmt "if(1) { }"                == Nothing
  , go parseStmt "if(1) { 2; }"             == Just (S.If (S.Int 1) [(S.Expr (S.Int 2))])
  , go parseStmt "if(0) { 1; 2; 3; }"       == Just (S.If (S.Int 0) [(S.Expr (S.Int 1)), (S.Expr (S.Int 2)), (S.Expr (S.Int 3))])
  , go parseStmt "if(0) { 1; } else { 2; }" == Just (S.IfElse (S.Int 0) [(S.Expr (S.Int 1))] [(S.Expr (S.Int 2))])
  , go parseStmt "while(x) { x = x + 1; }"  == Just (S.While (S.Id "x") [S.Expr (S.Assign "x" (S.Add (S.Id "x") (S.Int 1)))])
  -- Functions
  , go parseFunc "f() { return 0; }"               == Just (S.Func "f" [] [] [S.Return (S.Int 0)])
  , go parseFunc "f() { x = 0; return x; }"        == Just (S.Func "f" [] [] [S.Expr (S.Assign "x" (S.Int 0)), S.Return (S.Id "x")])
  , go parseFunc "f() { int x; return 0; }"        == Just (S.Func "f" [] ["x"] [S.Return (S.Int 0)])
  , go parseFunc "f() { int x; int y; return 0; }" == Just (S.Func "f" [] ["x", "y"] [S.Return (S.Int 0)])
  , go parseFunc "f(int x) { return 0; }"          == Just (S.Func "f" ["x"] [] [S.Return (S.Int 0)])
  , go parseFunc "f(int x, int y) { return 0; }"   == Just (S.Func "f" ["x", "y"] [] [S.Return (S.Int 0)])
  , go parseFunc "f(int x) { int y; return 0; }"   == Just (S.Func "f" ["x"] ["y"] [S.Return (S.Int 0)])
  ]
  where
  go f str = do
    ts <- scan str
    f ts

convertTests :: [Bool]
convertTests = 
  -- Expressions
  [ goExpr "2 + 2 + 2"                 == Just (S.empty,                      [I.Int 2, I.Int 2, I.Add, I.Int 2, I.Add])
  , goExpr "2 + (2 + 2)"               == Just (S.empty,                      [I.Int 2, I.Int 2, I.Int 2, I.Add, I.Add])
  , goExpr "x + a"                     == Just (S.fromList ["a"],             [I.Local 2, I.Global "a", I.Add])
  , goExpr "x = getchar()"             == Just (S.fromList ["getchar"],       [I.Global "getchar", I.Call 0, I.Assign 2])
  , goExpr "x = y = getc(stdin)"       == Just (S.fromList ["getc", "stdin"], [I.Global "stdin", I.Global "getc", I.Call 1, I.Assign 1, I.Assign 2])
  , goExpr "f(1, 2, g(10, 20, 30), 4)" == Just (S.fromList ["f", "g"],        [I.Int 4, I.Int 30, I.Int 20, I.Int 10, I.Global "g", I.Call 3, I.Int 2, I.Int 1, I.Global "f", I.Call 4])
  -- Statements
  , goStmt "2 + 2;"                   == Just [I.Int 2, I.Int 2, I.Add, I.Discard]
  , goStmt "if(1) { return 0; }"      == Just [I.Int 1, I.JumpZero 0, I.Int 0, I.Return 3, I.Label 0]
  , goStmt "if(0) { 1; } else { 0; }" == Just [I.Int 0, I.JumpZero 0, I.Int 1, I.Discard, I.Jump 1, I.Label 0, I.Int 0, I.Discard, I.Label 1]
  , goStmt "while(x) { x = f(x); }"   == Just [I.Label 0, I.Local 2, I.JumpZero 1, I.Local 2, I.Global "f", I.Call 1, I.Assign 2, I.Discard, I.Jump 0, I.Label 1]
  ]
  where
  goExpr str = do
    ts <- scan str
    e <- parseExpr ts
    swap <$> evalRWST (convertExpr e) (M.fromList [("x", 2), ("y", 1)]) (0, 0)

  goStmt str = do
    ts <- scan str
    s <- parseStmt ts
    fst <$> evalRWST (convertStmt s) (M.fromList [("x", 2), ("y", 1)]) (3, 0)

tests :: [Bool]
tests = lexTests ++ parseTests ++ convertTests

main :: IO ()
main =
  case findIndex not tests of
    Just i -> die $ unwords ["Test", show i, "failed."]
    Nothing -> putStrLn "All tests were successful."
