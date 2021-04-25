module Alpha where

import Gen
import KNormal

import Control.Monad.Identity
import Control.Monad.Reader
import qualified Data.Map as M

type Env = M.Map String String

find :: Monad m => String -> ReaderT Env (GenT m) String
find x = do
  env <- ask
  case M.lookup x env of
    Just x -> return x
    Nothing -> return x

genId :: Monad m => String -> ReaderT Env (GenT m) String
genId x = do
  y <- lift gen
  return $ unwords [x, show y]

alpha :: Monad m => KNormal -> GenT m KNormal
alpha e = runReaderT (go e) M.empty
  where
  go :: Monad m => KNormal -> ReaderT Env (GenT m) KNormal
  go Unit = return Unit
  go (Int x) = return $ Int x
  go (Float x) = return $ Float x
  go (Neg v) = Neg <$> find v
  go (Add v1 v2) = do v1 <- find v1; v2 <- find v2; return $ Add v1 v2
  go (Sub v1 v2) = do v1 <- find v1; v2 <- find v2; return $ Sub v1 v2
  go (FNeg v) = FNeg <$> find v
  go (FAdd v1 v2) = do v1 <- find v1; v2 <- find v2; return $ FAdd v1 v2
  go (FSub v1 v2) = do v1 <- find v1; v2 <- find v2; return $ FSub v1 v2
  go (FMul v1 v2) = do v1 <- find v1; v2 <- find v2; return $ FMul v1 v2
  go (FDiv v1 v2) = do v1 <- find v1; v2 <- find v2; return $ FDiv v1 v2
  go (IfEq v1 v2 e1 e2) = do
    v1 <- find v1
    v2 <- find v2
    e1 <- go e1
    e2 <- go e2
    return $ IfEq v1 v2 e1 e2
  go (IfLE v1 v2 e1 e2) = do
    v1 <- find v1
    v2 <- find v2
    e1 <- go e1
    e2 <- go e2
    return $ IfLE v1 v2 e1 e2
  go (Let v e1 e2) = do
    v' <- genId v
    e1 <- go e1
    e2 <- local (M.insert v v') $ go e2
    return $ Let v' e1 e2
  go (Var v) = Var <$> find v
  go (LetRec v vs e1 e2) = do
    v' <- genId v
    local (M.insert v v') $ do
      vs' <- traverse genId vs
      e1 <- local (M.union (M.fromList (zip vs vs'))) $ go e1
      e2 <- go e2
      return $ LetRec v' vs' e1 e2
  go (App v vs) = do
    v <- find v
    vs <- traverse find vs
    return $ App v vs
  go (Tuple vs) = do
    vs <- traverse find vs
    return $ Tuple vs
  go (LetTuple xs v e) = do
    xs' <- traverse genId xs
    v <- find v
    e <- local (M.union (M.fromList (zip xs xs'))) $ go e
    return $ LetTuple xs' v e
  go (Get v1 v2) = do v1 <- find v1; v2 <- find v2; return $ Get v1 v2
  go (Put v1 v2 v3) = do v1 <- find v1; v2 <- find v2; v3 <- find v3; return $ Put v1 v2 v3
  go (ExtArray x) = return $ ExtArray x
  go (ExtFunApp x vs) = do
    vs <- traverse find vs
    return $ ExtFunApp x vs

test :: KNormal -> KNormal
test e = runIdentity . runGenT $ alpha e

tests :: [KNormal]
tests = map test
  [ Unit
  , Int 10
  , Float 10
  , Let "a" Unit Unit
  , Neg "a"
  , Let "a" Unit (Neg "a")
  , Let "a" Unit (Neg "b")
  , Let "a" Unit (Add "a" "a")
  , Let "a" Unit (Add "a" "b")
  , Let "a" Unit (Let "b" Unit (Add "a" "b"))
  , Let "a" Unit (Sub "a" "a")
  , Let "a" Unit (Sub "a" "b")
  , Let "a" Unit (Let "b" Unit (Sub "a" "b"))
  , Let "a" Unit (Let "b" Unit (IfEq "a" "b" Unit Unit))
  , Let "a" Unit (Let "b" Unit (IfEq "a" "b" (Let "a" Unit (Neg "a")) Unit))
  , Let "a" Unit (Let "a" Unit (Var "a"))
  , LetRec "f" [] (Var "f") (Var "f")
  , Let "a" Unit (LetRec "f" ["a"] (Var "a") (Tuple ["f", "a"]))
  , Let "a" Unit (LetRec "f" ["a"] (Var "a") (App "f" ["a"]))
  , Let "x" Unit (LetTuple ["a", "b", "c"] "x" (Tuple ["a", "b", "c"]))
  , Let "a" Unit (Get "a" "a")
  , Let "a" Unit (Put "a" "a" "a")
  , Let "a" Unit (ExtArray "a")
  , Let "x" Unit (LetTuple ["a", "b", "c"] "x" (ExtFunApp "a" ["a", "b", "c"]))
  ]

main :: IO ()
main = do
  traverse print tests
  return ()
