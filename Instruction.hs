module Instruction where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString

data Instruction
  = Int Int
  | Call Int
  | Neg
  | BitwiseNot
  | Not
  | Mul
  | Div
  | Mod
  | Add
  | Sub
  | ShiftLeft
  | ShiftRight
  | LT'
  | GT'
  | LE
  | GE
  | EQ'
  | NE
  | BitwiseAnd
  | BitwiseXor
  | BitwiseOr
  deriving (Eq, Show)
