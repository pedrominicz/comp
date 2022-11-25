module Nameless (Expr(..), nameless) where

import qualified ANF as N

import Data.List

data Expr
  = Var {-# UNPACK #-} !Int
  | Lam Expr
  | App {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | Let Expr Expr
  deriving (Eq, Show)

nameless :: N.Expr -> Maybe Expr
nameless = go []
  where
  go :: [N.Name] -> N.Expr -> Maybe Expr
  go ctx (N.AExpr (N.Var x)) = Var <$> elemIndex x ctx
  go ctx (N.AExpr (N.Lam x b)) = Lam <$> go (x : ctx) b
  go ctx (N.CExpr (N.App f a)) = App <$> elemIndex f ctx <*> elemIndex a ctx
  go ctx (N.Let x e1 e2) = Let <$> go ctx e1 <*> go (x : ctx) e2
