module Convert where

import qualified Instruction as I
import qualified Syntax as S

import Control.Monad.RWS
import Control.Monad.Trans
import Data.Traversable
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import qualified Data.Set as S

type Env = M.Map B.ByteString Int

next :: RWST Env (S.Set B.ByteString) Int Maybe Int
next = do
  x <- get
  modify succ
  return x

convertExpr :: S.Expr -> RWST Env (S.Set B.ByteString) Int Maybe [I.Instruction]
convertExpr e = case e of
  S.Id i -> do
    env <- ask
    case M.lookup i env of
      Just i -> return $ [I.Local i]
      Nothing -> do
        tell $ S.singleton i
        return $ [I.Global i]
  S.Int x -> return $ [I.Int x]
  S.Call e es -> do
    es <- for (e:es) convertExpr
    return $ concat (reverse es) ++ [I.Call (length es - 1)]
  S.Add e1 e2 -> do
    e1 <- convertExpr e1
    e2 <- convertExpr e2
    return $ e1 ++ e2 ++ [I.Add]
  S.Assign i e -> do
    env <- ask
    e <- convertExpr e
    case M.lookup i env of
      Just i -> return $ e ++ [I.Assign i]
      Nothing -> lift Nothing

convertStmt :: S.Stmt -> RWST Env (S.Set B.ByteString) Int Maybe [I.Instruction]
convertStmt s = case s of
  S.Expr e -> do
    e <- convertExpr e
    return $ e ++ [I.Discard]
  S.If e ss -> do
    end <- next
    e <- convertExpr e
    ss <- concat <$> for ss convertStmt
    return $ e ++ [I.JumpZero end] ++ ss ++ [I.Label end]
  S.IfElse e ss1 ss2 -> do
    else' <- next
    end <- next
    e <- convertExpr e
    ss1 <- concat <$> for ss1 convertStmt
    ss2 <- concat <$> for ss2 convertStmt
    return $ e ++ [I.JumpZero else'] ++
      ss1 ++ [I.Jump end, I.Label else'] ++ ss2 ++ [I.Label end]
  S.While e ss -> do
    start <- next
    end <- next
    e <- convertExpr e
    ss <- concat <$> for ss convertStmt
    return $ [I.Label start] ++ e ++ [I.JumpZero end] ++
      ss ++ [I.Jump start, I.Label end]
  S.Return e -> do
    e <- convertExpr e
    return $ e ++ [I.Return]

convertFunc :: S.Func -> RWST Env (S.Set B.ByteString) Int Maybe [I.Instruction]
convertFunc (S.Func name args decl ss) = do
  let env = M.fromList $ zip (args ++ decl) [0..]
  ss <- local (const env) $ concat <$> for ss convertStmt
  return $ [I.Func name (length args) (length decl)] ++ ss ++ [I.Int 0, I.Return]
