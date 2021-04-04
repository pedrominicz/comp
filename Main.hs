module Main where

import Gen
import Name
import qualified Lexer as L
import qualified Parser as P
import qualified Syntax as S

import Control.Monad.Trans
import Data.Maybe
import System.IO

test :: String -> IO ()
test str = do
  let exp = fromJust . runGenT $ lift (L.lex str >>= P.parse) >>= nameTerm
  print exp

main :: IO ()
main = do
  test "()"
  test "true + false"
  test "-1"
  test "1. - -1."
  test "-.1. +. -1."
  test "1 -. 2 *. 3 /. 4"
  test "1 = 2 < 3"
  test "not true"
  test "if () then () else ()"
  test "let _ = () in ()"
  test "let _ = _ in _"
  test "a + _ + c + d"
  test "let rec const x _ = x in (); _"
  test "f _ _ c _ e"
  test "let (x, _) = _, _ in ()"
  test "Array.create _ _"
  test "_.(_)"
  test "_.(_) <- _"
