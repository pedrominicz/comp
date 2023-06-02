module Expr (Expr(..)) where

import qualified FlatParse.Basic as F

data Expr
  = Var {-# UNPACK #-} !F.Span
  | Lam {-# UNPACK #-} !F.Span Expr
  | App Expr Expr
