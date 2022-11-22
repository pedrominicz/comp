{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Expr (Expr(..)) where

import Control.DeepSeq
import GHC.Generics

data Expr
  = Var {-# UNPACK #-} !Int
  | Lam Expr
  | App Expr Expr
  deriving (Eq, Generic, NFData, Show)
