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
