module Syntax where

import Type

data Syntax
  = Unit
  | Bool Bool
  | Int Int
  | Float Float
  | Not Syntax
  | Neg Syntax
  | Add Syntax Syntax
  | Sub Syntax Syntax
  | FNeg Syntax
  | FAdd Syntax Syntax
  | FSub Syntax Syntax
  | FMul Syntax Syntax
  | FDiv Syntax Syntax
  | Eq Syntax Syntax
  | LE Syntax Syntax
  | If Syntax Syntax Syntax
  | Let String Type Syntax Syntax
  | Var String
  | LetRec String Type [(String, Type)] Syntax Syntax
  | App Syntax [Syntax]
  | Tuple [Syntax]
  | LetTuple [(String, Type)] Syntax Syntax
  | Array Syntax Syntax
  | Get Syntax Syntax
  | Put Syntax Syntax Syntax
  deriving (Eq, Read, Show)
