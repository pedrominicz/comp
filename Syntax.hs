module Syntax (Expr(..)) where

import Data.ByteString (ByteString)

data Expr
  = Var {-# UNPACK #-} !ByteString
  | Lam {-# UNPACK #-} !ByteString Expr
  | App Expr Expr
  | Let {-# UNPACK #-} !ByteString Expr Expr
  deriving (Eq, Show)
