{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Expr where

import Data.Kind

data ExprF :: Bool -> Type where
  Var :: Int -> ExprF a
  Lam :: ExprF a -> ExprF a
  App :: ExprF a -> ExprF a -> ExprF a

deriving instance Show (ExprF a)

type Expr = ExprF 'False
