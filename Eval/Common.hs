{-# LANGUAGE ScopedTypeVariables #-}

module Eval.Common where

import Expr

data Value
  = Closure Expr Env
  deriving Show

type Env = [Value]

-- https://hackage.haskell.org/package/zippers-0.3.2/docs/src/Control.Zipper.Internal.html#farthest
farthest :: forall a. (a -> Maybe a) -> a -> a
farthest f = go
  where
  go :: a -> a
  go x = maybe x go (f x)

{-# INLINE farthest #-}

-- This is not exactly reification because it doesn't necessarily return a
-- normal form.
reify :: Value -> Expr
reify (Closure e env) = go 0 (map reify env) e
  where
  go :: Int -> [Expr] -> Expr -> Expr
  go k env (Var x) =
    if x < k
      then Var x
      else env !! (x - k)
  go k env (Lam b) = Lam (go (k + 1) env b)
  go k env (App f a) = App (go k env f) (go k env a)
