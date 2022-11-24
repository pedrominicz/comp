{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Expr (Expr, ExprF(..)) where

import Control.DeepSeq
import Data.Kind

data ExprF :: Bool -> Type where
  Var :: {-# UNPACK #-} !Int -> ExprF b
  Lam :: ExprF b -> ExprF b
  App :: ExprF b -> ExprF b -> ExprF b
  Let :: ExprF 'False -> ExprF 'False -> ExprF 'False

deriving instance Eq (ExprF b)

deriving instance Show (ExprF b)

instance NFData (ExprF b) where
  rnf :: ExprF b -> ()
  rnf (Var x) = rnf x
  rnf (Lam b) = rnf b
  rnf (App f a) = rnf f `seq` rnf a
  rnf (Let e1 e2) = rnf e1 `seq` rnf e2

type Expr = ExprF 'False
