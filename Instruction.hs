module Instruction where

import qualified Data.ByteString.Lazy as B

data Instruction
  = Int Int
  | Call Int
  | Add
  | Local Int
  | Global B.ByteString
  | Assign Int
  | Discard
  | JumpZero Int
  | Jump Int
  | Label Int
  | Return
  | Func B.ByteString Int Int
  deriving (Eq, Show)
