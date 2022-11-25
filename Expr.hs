{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Expr (Expr(..)) where

import Control.DeepSeq
import GHC.Generics

data Expr
  = Var {-# UNPACK #-} !Int
  | Lam Expr
  | App {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  | Let Expr Expr
  deriving (Eq, Generic, NFData, Show)
