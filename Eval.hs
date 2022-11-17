{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}

module Eval (eval) where

import Expr

import Unsafe.Coerce

data Closure
  = Closure (ExprF 'True) Env
  | Level Int
  | Expr (ExprF 'False) Int
  deriving Show

pattern Lambda :: Closure
pattern Lambda <- Level _
  where
  Lambda = Level (error "unreachable")

{-# COMPLETE Closure, Lambda, Expr #-}

type Env = [Closure]

type Stack = [Closure]

type State = (Closure, Stack, Int)

compile :: ExprF 'False -> ExprF 'True
compile = unsafeCoerce

-- Crégut's strongly reducing Krivine abstract machine as described in Deriving
-- the Full-Reducing Krivine Machine from the Small-Step Operational Semantics
-- of Normal Order.
step :: State -> State
step (Closure e env, s, l) =
  case e of
    -- Unbound variables throw exceptions.
    Var x -> (env !! x, s, l)
    Lam b ->
      case s of
        c@(Closure _ _) : s -> (Closure b (c : env), s, l)
        _ -> (Closure b (Level (l + 1) : env), Lambda : s, l + 1)
    App f a -> (Closure f env, Closure a env : s, l)
step (Level l, s, l') = (Expr (Var (l' - l)) l', s, l')
step (Expr e l, c : s, l') =
  case c of
    Closure _ _ -> (c, Expr e l : s, l)
    Lambda -> (Expr (Lam e) l, s, l')
    Expr e' l -> (Expr (App e' e) l, s, l')
-- Final state.
step (Expr _ _, [], _) = error "unreachable"

final :: State -> Bool
final (Expr _ _, [], _) = True
final _ = False

eval :: Expr -> Expr
eval e =
  case until final step (Closure (compile e) [], [], 0) of
    (Expr e _, _, _) -> e
    _ -> error "unreachable"
