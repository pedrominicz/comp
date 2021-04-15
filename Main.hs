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

inferAux :: S.Syntax -> Maybe T.Type
inferAux e = evalStateT (infer e) IM.empty

test :: String -> IO ()
test str = do
  let typ = runGenT (lift (L.lex str >>= P.parse) >>= nameTerm) >>= inferAux
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
