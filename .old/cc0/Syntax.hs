module Syntax where

import qualified Data.ByteString.Lazy as B

data Expr
  -- Primary expressions
  = Id B.ByteString
  | Int Int
  -- Postfix expresssions
  | Call Expr [Expr]
  -- Binary expressions   
  | Add Expr Expr
  -- Assigment expressions
  | Assign B.ByteString Expr
  deriving (Eq, Show)

data Stmt
  = Expr Expr
  | If Expr [Stmt]
  | IfElse Expr [Stmt] [Stmt]
  | While Expr [Stmt]
  | Return Expr
  deriving (Eq, Show)

data Func = Func
  { funcName :: B.ByteString
  , funcArgs :: [B.ByteString]
  , funcDecl :: [B.ByteString]
  , funcBody :: [Stmt]
  } deriving (Eq, Show)
