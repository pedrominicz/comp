module Instruction where

import Data.ByteString.Lazy (ByteString)

data Instruction
  = Int Int
  -- | Call Int
  | Add
  deriving (Eq, Show)
