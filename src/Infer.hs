module Infer (infer) where

import Env
import Expr
import Type

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Data.ByteString (ByteString)
import qualified FlatParse.Basic as F

type Infer a = ReaderT Env (MaybeT IO) a

failed :: Infer a
failed = lift $ hoistMaybe Nothing

occurs :: Int -> Type -> Infer ()
occurs x = go
  where
  go :: Type -> Infer ()
  go (Ref x') =
    if x == x'
      then failed
      else typeOf x' >>= go
  go (Fun ta tb) = go ta >> go tb
  go Unknown = return ()

unify :: Type -> Type -> Infer ()
unify (Ref x1) (Ref x2) | x1 == x2 = return ()
unify (Ref x1) (Ref x2) = do
  t1 <- typeOf x1
  t2 <- typeOf x2
  case (t1, t2) of
    (Unknown, Unknown) -> bind x1 (Ref x2)
    (Unknown, t2) -> unify (Ref x1) t2
    (t1, Unknown) -> unify t1 (Ref x2)
    _ -> unify t1 t2
unify (Ref x1) t2 =
  typeOf x1 >>= \case
    Unknown -> occurs x1 t2 >> bind x1 t2
    t1 -> unify t1 t2
unify t1 (Ref x2) =
  typeOf x2 >>= \case
    Unknown -> occurs x2 t1 >> bind x2 t1
    t2 -> unify t1 t2
unify (Fun ta1 tb1) (Fun ta2 tb2) = unify ta1 ta2 >> unify tb1 tb2
unify _ _ = failed

deref :: Type -> Infer Type
deref (Ref x) =
  typeOf x >>= \case
    Unknown -> return (Ref x)
    t -> deref t
deref (Fun ta tb) = Fun <$> deref ta <*> deref tb
deref Unknown = return Unknown

runInfer :: Infer a -> IO (Maybe a)
runInfer x = new >>= runMaybeT . runReaderT x

data Ctx
  = Bind {-# UNPACK #-} !ByteString Type Ctx
  | Empty

typeOf' :: Ctx -> ByteString -> Maybe Type
typeOf' ctx x = go ctx
  where
  go :: Ctx -> Maybe Type
  go (Bind x' t ctx) =
    if x == x'
      then Just t
      else go ctx
  go Empty = Nothing

infer :: ByteString -> Expr -> IO (Maybe Type)
infer str e = runInfer $ go Empty e >>= deref
  where
  go :: Ctx -> Expr -> Infer Type
  go ctx (Var x) = lift . hoistMaybe $ typeOf' ctx (F.unsafeSlice str x)
  go ctx (Lam x b) = do
    ta <- fresh
    tb <- go (Bind (F.unsafeSlice str x) ta ctx) b
    return (Fun ta tb)
  go ctx (App f a) = do
    tf <- go ctx f
    ta <- go ctx a
    tb <- fresh
    unify tf (Fun ta tb)
    return tb
