module Type where

data Type
  = Unit
  | Bool
  | Int
  | Float
  -- Function are *not* curried.
  | Fun [Type] Type
  | Array Type
  | Var (Maybe Type)
  deriving (Eq, Read, Show)
