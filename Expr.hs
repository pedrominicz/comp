{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}

module Expr (Expr, ExprF(..)) where

import Control.DeepSeq
import GHC.Generics

data ExprF (b :: Bool)
  = Var Int
  | Lam (ExprF b)
  | App (ExprF b) (ExprF b)
  deriving (Eq, Generic, NFData, Show)

type Expr = ExprF 'False
