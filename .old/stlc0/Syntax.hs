module Syntax (Expr(..), Name) where

import Data.ByteString (ByteString)

type Name = (ByteString, Int)

data Expr
  = Var Name
  | Lam Name Expr
  | App Expr Expr
  | Let Name Expr Expr
  | Num Int
  | Add Expr Expr
  deriving (Eq, Show)
