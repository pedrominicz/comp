module Type where

data Type
  = Unit
  | Bool
  | Int
  | Float
  -- Function are *not* curried.
  | Fun [Type] Type
  | Array Type
  | Var Int
  deriving (Eq, Read, Show)
