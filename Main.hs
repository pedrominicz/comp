module Main where

import Type
import Typing

import Control.Monad.State
import qualified Data.IntMap as IM

test :: Type -> Type -> IO ()
test t1 t2 = print $ execStateT (unify t1 t2) IM.empty

v1, v2, v3 :: Type
v1 = Var 1
v2 = Var 2
v3 = Var 3

main :: IO ()
main = do
  test Unit Unit
  test Bool Bool
  test Int Int
  test Float Float
  test Unit Float
  test v1 Unit
  test Unit v1
  test (Tuple [v1, v2]) (Tuple [Unit, Float])
  test (Tuple [Unit, v2]) (Tuple [Unit, Float])
  test (Tuple [Unit, v2]) (Tuple [v1, Float])
  test (Tuple [Unit, v2]) (Fun [v1, Float] Unit)
  test (Array v1) Unit
  test (Array v1) (Array v2)
  test (Array Float) (Array v2)
  test (Tuple [v1, Int]) (Tuple [v2, v2])
  test (Tuple [v1, v3, v3]) (Tuple [v2, v2, Int])
  test (Tuple [v1, v3, v3]) (Tuple [v2, v2])
  test (Fun [] Unit) (Fun [] Unit)
  test (Fun [v1] Unit) (Fun [Int] Unit)
  test (Fun [v1, Int] Unit) (Fun [v2, v2] Unit)
  test (Fun [v1, v3, v3] Unit) (Fun [v2, v2, Int] Unit)
  test (Fun [v1, v3, v3] Unit) (Fun [v2, v2] Unit)
  test (Fun [v1, v2] Unit) (Fun [Unit, Float] Unit)
  test (Fun [Unit, v2] Unit) (Fun [Unit, Float] Unit)
  test (Fun [Unit, v2] Unit) (Fun [v1, Float] Unit)
