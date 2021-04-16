module Main where

import Gen
import Name
import Typing
import qualified Lexer as L
import qualified Parser as P
import qualified Syntax as S
import qualified Type as T

import Control.Monad.State
import qualified Data.IntMap as IM
import qualified Data.Map as M

aux :: S.Syntax -> GenT Maybe T.Type
aux e = evalStateT (infer M.empty e >>= derefType) (IM.empty, M.empty)

test :: String -> IO ()
test str = do
  let typ = runGenT $ lift (L.lex str >>= P.parse) >>= nameTerm >>= aux
  print typ

main :: IO ()
main = do
  test "10"
  test "10."
  test "true"
  test "false"
  test "()"
  test "not 10"
  test "not true"
  test "not false"
  test "-10"
  test "-10."
  test "10 + 1"
  test "10. + 1"
  test "true + 1"
  test "10. +. 2."
  test "10. -. 2."
  test "10 - 2"
  test "10. /. 2."
  test "10. *. 2."
  test "() = ()"
  test "2 = 3"
  test "2. = 3."
  test "true = true"
  test "10 = true"
  test "10 = 2."
  test "() = 2."
  test "if true then 10 else 2"
  test "if () then 10 else 2"
  test "if 2 = 2 then 2. else 3."
  test "if 2 = 2 then false else not (not true)"
  test "if true then 10 else true"
  test "if true then 10 else 2."
  test "if true then () else 2."
  test "10 < 2"
  test "10. < 2."
  test "let x = () in x"
  test "let x = 2 + 4 in x > 8"
  test "x"
  test "10; x"
  test "unit; x"
  test "let unit = () in unit; 10"
  test "let rec const x y = x in const"
  test "let rec f x y = () in f"
  test "let rec f x y = () in f"
  test "let rec f x y = x + y in f"
  test "let rec f x y = x +. y in f"
  test "let rec f x y = if x +. y = 0. then false else true in f"
  test "let rec f x y = if x +. y = 0. then () else () in f"
  test "let rec f x y = if x then y else 0 in f"
  test "f 10"
  test "f () true 1."
  -- Functions are *not* curried.
  test "let rec const x y = x in const 10"
  test "let rec const x y = x in const 10 ()"
  test "let rec const x y = x in const 1. true"
  test "let rec f x y = if x +. y = 0. then () else other in f 1. 2."
  test "(1, 2, 3)"
  test "(1, true, ())"
  test "let (x, y, z) = (1, true, ()) in x"
  test "let (x, y, z) = (1, true, ()) in y"
  test "let (x, y, z) = (1, true, ()) in z"
  test "Array.make 10 0"
  test "x.(0)"
  test "x.(0) <- 0"
  test "let a = Array.make 10 0 in a.(0) <- 10"
