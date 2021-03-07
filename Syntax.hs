module Syntax where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString

data Expr
  -- Primary expressions
  = Identifier ByteString
  | IntLiteral Int
  | StringLiteral ByteString
  -- Postfix expresssions
  | FunctionCall Expr [Expr]
  -- Unary expressions
  | AddressOf Expr        -- &
  | Indirection Expr      -- *
  | Negative Expr         -- -
  | BitwiseNot Expr       -- ~
  | Not Expr              -- !
  -- Binary expressions   
  | Mul Expr Expr         -- *
  | Div Expr Expr         -- /
  | Mod Expr Expr         -- %
  | Add Expr Expr         -- +
  | Sub Expr Expr         -- -
  | ShiftLeft Expr Expr   -- <<
  | ShiftRight Expr Expr  -- >>
  | LT' Expr Expr         -- < (avoids conflict with `Prelude.LT`)
  | GT' Expr Expr         -- > (avoids conflict with `Prelude.GT`)
  | LE Expr Expr          -- <=
  | GE Expr Expr          -- >=
  | EQ' Expr Expr         -- == (avoids conflict with `Prelude.EQ`)
  | NE Expr Expr          -- !=
  | BitwiseAnd Expr Expr  -- &
  | BitwiseXor Expr Expr  -- ^
  | BitwiseOr Expr Expr   -- |
  -- Assigment expressions
  | Assignment ByteString Expr
  deriving (Eq, Show)

data Stmt
  = Compound [ByteString] [Stmt]
  | Expression Expr
  | If Expr Stmt
  | IfElse Expr Stmt Stmt
  | While Expr Stmt
  deriving (Eq, Show)
