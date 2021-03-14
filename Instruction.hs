module Instruction where

import qualified Data.ByteString.Lazy as B

data Instruction
  = Int Int
  | Call Int
  | Add
  | Local Int
  | Global B.ByteString
  | Assign Int
  deriving (Eq, Show)
