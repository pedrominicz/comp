{-# LANGUAGE GADTs #-}

module Eval.Strict (eval) where

import Eval.Common
import Expr

data Stack
  = Value Value Stack
  | Pending Expr Env Stack
  | Halt
  deriving Show

type State = (Value, Stack)

-- Based on the Krivine machine with marks on the stack described in The ZINC
-- experiment: an economical implementation of the ML language. Every function
-- application is strict.
step :: State -> Maybe State
step (v@(Closure e env), s) =
  case e of
    -- Unbound variables throw exceptions.
    Var x -> Just (env !! x, s)
    Lam b ->
      case s of
        Value v s -> Just (Closure b (v : env), s)
        Pending e env s -> Just (Closure e env, Value v s)
        -- Final state.
        Halt -> Nothing
    App f a -> Just (Closure a env, Pending f env s)
    -- This way of dealing with let expressions is very ad hoc and differs from
    -- how the ZINC abstract machine does it.
    Let e1 e2 -> Just (Closure e1 env, Pending (Lam e2) env s)

eval :: Expr -> Expr
eval e = reify . fst $ farthest step (Closure e [], Halt)
