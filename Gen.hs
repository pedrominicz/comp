module Gen where

import Control.Applicative
import Control.Monad.Trans

newtype GenT m a = GenT { unGenT :: Int -> m (a, Int) }

instance Functor m => Functor (GenT m) where
  fmap f a = GenT $ fmap (\(a, s) -> (f a, s)) . unGenT a

instance Monad m => Applicative (GenT m) where
  pure a = GenT $ \s -> return (a, s)

  GenT f <*> GenT a = GenT $ \s -> do
    (f, s) <- f s
    (a, s) <- a s
    return (f a, s)

instance Monad m => Monad (GenT m) where
  a >>= f = GenT $ \s -> do
    (a, s) <- unGenT a s
    unGenT (f a) s

runGenT :: Monad m => GenT m a -> m a
runGenT a = do
  (a, _) <- unGenT a 0
  return a

gen :: Applicative m => GenT m Int
gen = GenT $ \s -> pure (s, s + 1)

instance MonadTrans GenT where
  lift a = GenT $ \s -> do
    a <- a
    return (a, s)

instance (Alternative m, Monad m) => Alternative (GenT m) where
  empty = GenT $ const empty

  GenT a1 <|> GenT a2 = GenT $ \s -> a1 s <|> a2 s
