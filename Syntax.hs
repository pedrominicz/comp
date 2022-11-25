module Syntax (Expr(..), Name) where

import Data.ByteString (ByteString)

type Name = (ByteString, Int)

data Expr
  = Var {-# UNPACK #-} !Name
  | Lam {-# UNPACK #-} !Name Expr
  | App Expr Expr
  | Let {-# UNPACK #-} !Name Expr Expr
  deriving (Eq, Show)
