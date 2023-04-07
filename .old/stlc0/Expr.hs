{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Expr (Expr(..)) where

import Control.DeepSeq
import GHC.Generics

data Expr
  = Var Int
  | Lam Expr
  | App Int Int
  | Let Expr Expr
  | Num Int
  | Add Int Int
  deriving (Eq, Generic, NFData, Show)
