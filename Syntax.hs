module Syntax where

import Type

import qualified Data.ByteString.Lazy as B

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
  | Let B.ByteString Type Syntax Syntax
  | Var B.ByteString
  | LetRec B.ByteString Type [(B.ByteString, Type)] Syntax Syntax
  | App Syntax [Syntax]
  | Tuple [Syntax]
  | LetTuple [(B.ByteString, Type)] Syntax Syntax
  | Array Syntax Syntax
  | Get Syntax Syntax
  | Put Syntax Syntax Syntax
  deriving (Eq, Read, Show)
