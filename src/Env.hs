module Env (Env, bind, fresh, new, typeOf) where

import Type

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import GHC.Exts
import GHC.IO (IO(..))

#include "MachDeps.h"

data Env = Env#
  {-# UNPACK #-} !(MutableByteArray# RealWorld)
  {-# UNPACK #-} !(MutVar# RealWorld (MutableArray# RealWorld Type))

new :: MonadIO m => m Env
new = liftIO $ IO \s -> case newByteArray# SIZEOF_HSINT# s of
  (# s, len #) -> case writeIntArray# len 0# 0# s of
    s -> case newArray# 8# Unknown s of
      (# s, arr #) -> case newMutVar# arr s of
        (# s, ref #) -> (# s, Env# len ref #)

{-# INLINE new #-}

typeOf :: MonadIO m => Int -> ReaderT Env m Type
typeOf (I# i) = ask >>= \(Env# _ ref) -> liftIO $ IO \s ->
  case readMutVar# ref s of (# s, arr #) -> readArray# arr i s

{-# INLINE typeOf #-}

bind :: MonadIO m => Int -> Type -> ReaderT Env m ()
bind (I# i) x = ask >>= \(Env# _ ref) -> liftIO $ IO \s ->
  case readMutVar# ref s of
    (# s, arr #) -> case writeArray# arr i x s of s -> (# s, () #)

{-# INLINE bind #-}

fresh :: MonadIO m => ReaderT Env m Type
fresh = ask >>= \(Env# len ref) -> liftIO $ IO \s ->
  case readIntArray# len 0# s of
    (# s, i #) -> case writeIntArray# len 0# (i +# 1#) s of
      s -> case readMutVar# ref s of
        (# s, arr #) ->
          let capacity = sizeofMutableArray# arr in
          case i ==# capacity of
            0# -> (# s, Ref (I# i) #)
            _ -> case newArray# (capacity *# 2#) Unknown s of
              (# s, arr' #) ->
                case copyMutableArray# arr 0# arr' 0# capacity s of
                  s -> case writeMutVar# ref arr' s of
                    s -> (# s, Ref (I# i) #)

{-# INLINE fresh #-}
