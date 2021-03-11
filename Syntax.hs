module Syntax where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString

data Expr
  -- Primary expressions
  = Id ByteString
  | Int Int
  -- Postfix expresssions
  | Call Expr [Expr]
  -- Binary expressions   
  | Add Expr Expr
  -- Assigment expressions
  | Assign ByteString Expr
  deriving (Eq, Show)

data Stmt
  = Expr Expr
  | If Expr [Stmt]
  | IfElse Expr [Stmt] [Stmt]
  | While Expr [Stmt]
  | Return Expr
  deriving (Eq, Show)

data Func = Func
  { funcName :: ByteString
  , funcArgs :: [ByteString]
  , funcDecl :: [ByteString]
  , funcBody :: [Stmt]
  } deriving (Eq, Show)
