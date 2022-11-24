{-# LANGUAGE GADTs #-}

module Eval.Krivine (eval) where

import Eval.Common
import Expr

type Stack = [Value]

type State = (Value, Stack)

-- Based on the Krivine machine as described in The ZINC experiment: an
-- economical implementation of the ML language.
step :: State -> Maybe State
step (Closure e env, s) =
  case e of
    -- Unbound variables throw exceptions.
    Var x -> Just (env !! x, s)
    Lam b ->
      case s of
        c : s -> Just (Closure b (c : env), s)
        -- Final state.
        [] -> Nothing
    App f a -> Just (Closure f env, Closure a env : s)
    Let e1 e2 -> Just (Closure e2 (Closure e1 env : env), s)

eval :: Expr -> Expr
eval e = reify . fst $ farthest step (Closure e [], [])
