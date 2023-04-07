{-# LANGUAGE ScopedTypeVariables #-}

module Eval (eval) where

import Expr

data Value
  = Closure Expr Env
  | Number {-# UNPACK #-} !Int
  deriving (Eq, Show)

type Env = [Value]

data Stack
  = Continue Expr Env Stack
  | Halt
  deriving (Eq, Show)

type State = (Expr, Env, Stack)

continue :: Value -> Stack -> Either State Value
continue v (Continue e env s) = Left (e, v : env, s)
continue v Halt = Right v

apply :: Value -> Value -> Stack -> Either State Value
apply (Closure b env) v s = Left (b, v : env, s)
apply _ _ _ = error "Eval.apply"

add :: Value -> Value -> Value
add (Number n1) (Number n2) = Number (n1 + n2)
add _ _ = error "Eval.add"

step :: State -> Either State Value
step (Var x, env, s) = continue (env !! x) s
step (Lam b, env, s) = continue (Closure b env) s
step (App f a, env, s) = apply (env !! f) (env !! a) s
step (Let e1 e2, env, s) = Left (e1, env, Continue e2 env s)
step (Num n, _, s) = continue (Number n) s
step (Add x1 x2, env, s) = continue (add (env !! x1) (env !! x2)) s

loop :: forall a b. (a -> Either a b) -> a -> b
loop f = go
  where
  go :: a -> b
  go x = either go id (f x)

eval :: Expr -> Value
eval e = loop step (e, [], Halt)
