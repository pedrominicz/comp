module Compile (Expr(..), compile) where

import ANF (normalize)
import Nameless
import Parse

import Data.ByteString (ByteString)

compile :: ByteString -> Maybe Expr
compile str = parse str >>= nameless . normalize
