{-# LANGUAGE ScopedTypeVariables #-}

module Eval (eval) where

import Compile

data Value
  = Closure Expr Env
  deriving Show

type Env = [Value]

data Stack
  = Continue Expr Env Stack
  | Halt
  deriving Show

type State = (Expr, Env, Stack)

continue :: Value -> Stack -> Either State Value
continue v (Continue e env s) = Left (e, v : env, s)
continue v Halt = Right v

apply :: Value -> Value -> Stack -> Either State Value
apply (Closure b env) v s = Left (b, v : env, s)

step :: State -> Either State Value
step (Var x, env, s) = continue (env !! x) s
step (Lam b, env, s) = continue (Closure b env) s
step (App f a, env, s) = apply (env !! f) (env !! a) s
step (Let e1 e2, env, s) = Left (e1, env, Continue e2 env s)

loop :: forall a b. (a -> Either a b) -> a -> b
loop f = go
  where
  go :: a -> b
  go x = either go id (f x)

eval :: Expr -> Value
eval e = loop step (e, [], Halt)
