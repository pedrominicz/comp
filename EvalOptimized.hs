{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}

module EvalOptimized (eval) where

import Expr

import Unsafe.Coerce

type Level = Int

data Closure
  = Closure (ExprF 'True) Env
  | Level Level
  | Expr (ExprF 'False)
  deriving Show

pattern Lambda :: Closure
pattern Lambda <- Level _
  where
  Lambda = Level (error "unreachable")

{-# COMPLETE Closure, Lambda, Expr #-}

type Env = [Closure]

type Stack = [Closure]

type State = (Closure, Stack, Level)

compile :: ExprF 'False -> ExprF 'True
compile = unsafeCoerce

access :: Level -> Int -> Env -> Closure
access l x [] = Expr (Var (x + l))
access _ 0 (c : _) = c
access l x (_ : env) = access l (x - 1) env

-- Optimized strongly reducing Krivine abstract machine based on the paper
-- Deriving the Full-Reducing Krivine Machine from the Small-Step Operational
-- Semantics of Normal Order.
--
-- A bit slower than `Eval.eval`.
step :: State -> State
step (Closure e env, s, l) =
  case e of
    -- Unbound variables throw exceptions.
    Var x -> (access l x env, s, l)
    Lam b ->
      case s of
        c@(Closure _ _) : s -> (Closure b (c : env), s, l)
        _ -> (Closure b (Level (l + 1) : env), Lambda : s, l + 1)
    App f a -> (Closure f env, Closure a env : s, l)
step (Level l', s, l) = (Expr (Var (l - l')), s, l)
step (Expr e, c : s, l) =
  case c of
    Closure _ _ -> (c, Expr e : s, l)
    Lambda -> (Expr (Lam e), s, l - 1)
    Expr e' -> (Expr (App e' e), s, l)
-- Final state.
step (Expr _, [], _) = error "unreachable"

final :: State -> Bool
final (Expr _, [], _) = True
final _ = False

eval :: Expr -> Expr
eval e =
  case until final step (Closure (compile e) [], [], 0) of
    (Expr e, _, _) -> e
    _ -> error "unreachable"
