module Typing where

import Gen
import qualified Syntax as S
import qualified Type as T

import Control.Monad.State
import Data.Foldable
import Data.Traversable
import qualified Data.IntMap as IM
import qualified Data.Map as M

type Env = M.Map String T.Type

type Typing a = StateT (IM.IntMap T.Type, Env) (GenT Maybe) a

derefType :: T.Type -> Typing T.Type
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
    env <- fst <$> get
    case IM.lookup x env of
      -- Uninstantiated type variables assumed to be `T.Int`.
      Nothing -> return T.Int
      Just t -> do
        t <- derefType t
        return t
  t -> return t

derefTerm :: S.Syntax -> Typing S.Syntax
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

bind :: Int -> T.Type -> Typing ()
bind v t = modify (\(env1, env2) -> (IM.insert v t env1, env2))

apply :: T.Type -> Typing T.Type
apply t = case t of
  T.Fun ts t -> do
    ts <- traverse apply ts
    t <- apply t
    return $ T.Fun ts t
  T.Tuple ts -> do
    ts <- traverse apply ts
    return $ T.Tuple ts
  T.Array t -> do
    t <- apply t
    return $ T.Array t
  T.Var v -> do
    env <- fst <$> get
    case IM.lookup v env of
      Nothing -> return $ T.Var v
      Just t -> do
        guard $ not (occur v t)
        t <- apply t
        bind v t
        return t
  t -> return t

unify :: T.Type -> T.Type -> Typing ()
unify t1 t2 = do
  t1 <- apply t1
  t2 <- apply t2
  case (t1, t2) of
    (t1, t2) | t1 == t2 -> return ()
    (T.Fun ts1 t1, T.Fun ts2 t2) -> do
      ts <- zipExact ts1 ts2
      traverse (uncurry unify) ts
      unify t1 t2
    (T.Tuple ts1, T.Tuple ts2) -> do
      ts <- zipExact ts1 ts2
      traverse_ (uncurry unify) ts
    (T.Array t1, T.Array t2) -> unify t1 t2
    (T.Var v, t) -> bind v t
    (t, T.Var v) -> bind v t
    _ -> mzero

zipExact :: MonadPlus m => [a] -> [b] -> m [(a, b)]
zipExact [] [] = return []
zipExact (x:xs) (y:ys) = ((x, y) :) <$> zipExact xs ys
zipExact _ _ = mzero

infer :: Env -> S.Syntax -> Typing T.Type
infer env e = case e of
  S.Unit -> return T.Unit
  S.Bool _ -> return T.Bool
  S.Int _ -> return T.Int
  S.Float _ -> return T.Float
  S.Not e -> infer env e >>= unify T.Bool >> return T.Bool
  S.Neg e -> infer env e >>= unify T.Int >> return T.Int
  S.Add e1 e2 -> do infer env e1 >>= unify T.Int; infer env e2 >>= unify T.Int; return T.Int
  S.Sub e1 e2 -> do infer env e1 >>= unify T.Int; infer env e2 >>= unify T.Int; return T.Int
  S.FNeg e -> infer env e >>= unify T.Float >> return T.Float
  S.FAdd e1 e2 -> do infer env e1 >>= unify T.Float; infer env e2 >>= unify T.Float; return T.Float
  S.FSub e1 e2 -> do infer env e1 >>= unify T.Float; infer env e2 >>= unify T.Float; return T.Float
  S.FMul e1 e2 -> do infer env e1 >>= unify T.Float; infer env e2 >>= unify T.Float; return T.Float
  S.FDiv e1 e2 -> do infer env e1 >>= unify T.Float; infer env e2 >>= unify T.Float; return T.Float
  S.Eq e1 e2 -> do
    t1 <- infer env e1
    t2 <- infer env e2
    unify t1 t2
    return T.Bool
  S.LE e1 e2 -> do
    t1 <- infer env e1
    t2 <- infer env e2
    unify t1 t2
    return T.Bool
  S.If e1 e2 e3 -> do
    infer env e1 >>= unify T.Bool
    t2 <- infer env e2
    t3 <- infer env e3
    unify t2 t3
    return t2
  S.Let x t e1 e2 -> do
    t1 <- infer env e1
    unify t1 t
    infer (M.insert x t env) e2
  S.Var x ->
    case M.lookup x env of
      Just t -> return t
      Nothing -> do
        env <- snd <$> get
        case M.lookup x env of
          Just t -> return t
          Nothing -> do
            t <- T.Var <$> lift gen
            modify (\(env1, env2) -> (env1, M.insert x t env2))
            return t
  S.LetRec name t args body e -> do
    -- Instead of creating `Name.hs`, new types variables could be `gen`erated
    -- here.
    let env' = M.insert name t env
    -- This may misbehave on edge cases, for example, if the the function has
    -- multiple arguments with the same name.
    let bodyEnv = M.fromList args `M.union` env'
    tBody <- infer bodyEnv body
    unify t (T.Fun (map snd args) tBody)
    infer env' e
  S.App fn args -> do
    ts <- traverse (infer env) args
    t <- T.Var <$> lift gen
    tf <- infer env fn
    unify tf (T.Fun ts t)
    return t
  S.Tuple es -> do
    ts <- traverse (infer env) es
    return $ T.Tuple ts
  S.LetTuple xs es e -> do
    ts <- infer env es
    unify (T.Tuple (map snd xs)) ts
    let env' = M.fromList xs `M.union` env
    infer env' e
  S.Array e1 e2 -> do
    t1 <- infer env e1
    unify T.Int t1
    t2 <- infer env e2
    return $ T.Array t2
  S.Get e1 e2 -> do
    t <- T.Var <$> lift gen
    t1 <- infer env e1
    unify (T.Array t) t1
    t2 <- infer env e2
    unify T.Int t2
    return t
  S.Put e1 e2 e3 -> do
    t <- infer env e3
    t1 <- infer env e1
    unify (T.Array t) t1
    t2 <- infer env e2
    unify T.Int t2
    return T.Unit

typing :: S.Syntax -> GenT Maybe (S.Syntax, Env)
typing e = do
  (e, (_, env)) <- flip runStateT (IM.empty, M.empty) $ do
    t <- infer M.empty e
    unify T.Unit t
    derefTerm e
  return (e, env)
