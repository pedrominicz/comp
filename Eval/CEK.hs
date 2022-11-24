{-# LANGUAGE GADTs #-}

module Eval.CEK (eval) where

import Eval.Common
import Expr

data Stack
  = PendingFun Expr Env Stack
  | PendingArg Expr Env Stack
  | Halt
  deriving Show

type State = (Value, Stack)

-- Based on `https://matt.might.net/articles/cek-machines/`.
step :: State -> Maybe State
step (v@(Closure e env), s) =
  case e of
    -- Unbound variables throw exceptions.
    Var x -> Just (env !! x, s)
    Lam b ->
      case s of
        PendingFun e env s -> Just (Closure e (v : env), s)
        PendingArg e env' s -> Just (Closure e env', PendingFun b env s)
        -- Final state.
        Halt -> Nothing
    App f a -> Just (Closure f env, PendingArg a env s)
    Let e1 e2 -> Just (Closure e1 env, PendingFun e2 env s)

eval :: Expr -> Expr
eval e = reify . fst $ farthest step (Closure e [], Halt)
