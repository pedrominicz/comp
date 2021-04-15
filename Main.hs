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
