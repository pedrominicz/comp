module Typing where

import qualified Syntax as S
import qualified Type as T

import Control.Monad.State
import Data.Traversable
import qualified Data.IntMap as IM

derefType :: T.Type -> StateT (IM.IntMap T.Type) Maybe T.Type
derefType t = case t of
  T.Fun ts t -> do
    ts <- traverse derefType ts
    t <- derefType t
    return $ T.Fun ts t
  T.Tuple ts -> do
    ts <- traverse derefType ts
    return $ T.Tuple ts
  T.Array t -> do
    t <- derefType t
    return $ T.Array t
  T.Var x -> do
    env <- get
    case IM.lookup x env of
      -- Uninstantiated type variables assumed to be `T.Int`.
      Nothing -> return T.Int
      Just t -> do
        t <- derefType t
        return t
  t -> return t

derefTerm :: S.Syntax -> StateT (IM.IntMap T.Type) Maybe S.Syntax
derefTerm e = case e of
  S.Not e -> do e <- derefTerm e; return $ S.Not e
  S.Neg e -> do e <- derefTerm e; return $ S.Neg e
  S.Add e1 e2 -> do e1 <- derefTerm e1; e2 <- derefTerm e2; return $ S.Add e1 e2
  S.Sub e1 e2 -> do e1 <- derefTerm e1; e2 <- derefTerm e2; return $ S.Sub e1 e2
  S.FNeg e -> do e <- derefTerm e; return $ S.FNeg e
  S.FAdd e1 e2 -> do e1 <- derefTerm e1; e2 <- derefTerm e2; return $ S.FAdd e1 e2
  S.FSub e1 e2 -> do e1 <- derefTerm e1; e2 <- derefTerm e2; return $ S.FSub e1 e2
  S.FMul e1 e2 -> do e1 <- derefTerm e1; e2 <- derefTerm e2; return $ S.FMul e1 e2
  S.FDiv e1 e2 -> do e1 <- derefTerm e1; e2 <- derefTerm e2; return $ S.FDiv e1 e2
  S.Eq e1 e2 -> do e1 <- derefTerm e1; e2 <- derefTerm e2; return $ S.Eq e1 e2
  S.LE e1 e2 -> do e1 <- derefTerm e1; e2 <- derefTerm e2; return $ S.LE e1 e2
  S.If e1 e2 e3 -> do e1 <- derefTerm e1; e2 <- derefTerm e2; e3 <- derefTerm e3; return $ S.If e1 e2 e3
  S.Let x t e1 e2 -> do t <- derefType t; e1 <- derefTerm e1; e2 <- derefTerm e2; return $ S.Let x t e1 e2
  S.LetRec x t ys e1 e2 -> do
    t <- derefType t
    ys <- for ys $ \(x, t) -> do
      t <- derefType t
      return (x, t)
    e1 <- derefTerm e1
    e2 <- derefTerm e2
    return $ S.LetRec x t ys e1 e2
  S.App e es -> do e <- derefTerm e; es <- traverse derefTerm es; return $ S.App e es
  S.Tuple es -> do es <- traverse derefTerm es; return $ S.Tuple es
  S.LetTuple xs e1 e2 -> do
    xs <- for xs $ \(x, t) -> do
      t <- derefType t
      return (x, t)
    e1 <- derefTerm e1
    e2 <- derefTerm e2
    return $ S.LetTuple xs e1 e2
  S.Array e1 e2 -> do e1 <- derefTerm e1; e2 <- derefTerm e2; return $ S.Array e1 e2
  S.Get e1 e2 -> do e1 <- derefTerm e1; e2 <- derefTerm e2; return $ S.Get e1 e2
  S.Put e1 e2 e3 -> do e1 <- derefTerm e1; e2 <- derefTerm e2; e3 <- derefTerm e3; return $ S.Put e1 e2 e3
  e -> return e

occur :: Int -> T.Type -> Bool
occur x t = case t of
  T.Fun ts t -> any (occur x) (t:ts)
  T.Tuple ts -> any (occur x) ts
  T.Array t -> occur x t
  T.Var y | x == y -> True
  _ -> False
