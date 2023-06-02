module Type (Type(..), pretty) where

import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Data.Char
import Data.List (elemIndex)

data Type
  = Fun Type Type
  | Ref {-# UNPACK #-} !Int
  | Unknown

data Array = Array {-# UNPACK #-} !Int [Int]

pretty :: Type -> IO ()
pretty t = evalStateT (go t) (Array 0 []) >> putChar '\n'
  where
  go :: Type -> StateT Array IO ()
  go (Ref x) = do
    Array len arr <- get
    case elemIndex x arr of
      Just i -> liftIO $ ref (len - i - 1)
      Nothing -> do
        put $ Array (len + 1) (x : arr)
        liftIO $ ref len
  go (Fun ta tb) = paren ta >> liftIO (putStr " → ") >> go tb
  go Unknown = liftIO $ putChar '?'

  paren :: Type -> StateT Array IO ()
  paren t@(Fun _ _) = liftIO (putChar '(') >> go t >> liftIO (putChar ')')
  paren t = go t

  -- FIXME: doesn't work if there are too many type variables.
  ref :: Int -> IO ()
  ref x | x < 26 = putChar (chr (ord 'A' + x))
  ref x | x < 52 = putChar (chr (ord 'A' + x - 26)) >> putChar '1'
  ref x = putChar (chr (ord 'A' + x - 52)) >> putChar '2'
