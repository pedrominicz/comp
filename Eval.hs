{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}

module Eval (eval) where

import Eval.Common (farthest)
import Expr

import Unsafe.Coerce

type Level = Int

data Closure
  = Closure (ExprF 'False) Env
  | Level {-# UNPACK #-} !Level
  | Expr (ExprF 'True) {-# UNPACK #-} !Level
  deriving Show

pattern Lambda :: Closure
pattern Lambda <- Level _
  where
  Lambda = Level 0

{-# COMPLETE Closure, Lambda, Expr #-}

type Env = [Closure]

type Stack = [Closure]

type State = (Closure, Stack, Level)

-- Crégut's strongly reducing Krivine abstract machine as described in Deriving
-- the Full-Reducing Krivine Machine from the Small-Step Operational Semantics
-- of Normal Order.
step :: State -> Maybe State
step (Closure e env, s, l) = Just $
  case e of
    -- Unbound variables throw exceptions.
    Var x -> (env !! x, s, l)
    Lam b ->
      case s of
        c@(Closure _ _) : s -> (Closure b (c : env), s, l)
        _ -> (Closure b (Level (l + 1) : env), Lambda : s, l + 1)
    App f a -> (Closure f env, Closure a env : s, l)
    Let e1 e2 -> (Closure e2 (Closure e1 env : env), s, l)
step (Level l', s, l) = Just (Expr (Var (l - l')) l, s, l)
step (Expr e l', c : s, l) = Just $
  case c of
    Closure _ _ -> (c, Expr e l' : s, l')
    Lambda -> (Expr (Lam e) l', s, l)
    Expr e' l' -> (Expr (App e' e) l', s, l)
-- Final state.
step (Expr _ _, [], _) = Nothing

eval :: Expr -> Expr
eval e =
  case farthest step (Closure e [], [], 0) of
    (Expr e _, _, _) -> unsafeCoerce e
    _ -> error "unreachable"
