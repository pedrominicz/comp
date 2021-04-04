module Name where

import Gen
import qualified Syntax as S
import qualified Type as T

import Data.Traversable

nameType :: Monad m => T.Type -> GenT m T.Type
nameType t = case t of
  T.Fun ts t -> do
    ts <- traverse nameType ts
    t <- nameType t
    return $ T.Fun ts t
  T.Tuple ts -> do
    ts <- traverse nameType ts
    return $ T.Tuple ts
  T.Array t -> do
    t <- nameType t
    return $ T.Array t
  T.Var _ -> do
    x <- gen
    return $ T.Var x
  t -> return t

nameIdent :: Monad m => String -> GenT m String
nameIdent "" = show <$> gen
nameIdent x = return x

nameTerm :: Monad m => S.Syntax -> GenT m S.Syntax
nameTerm e = case e of
  S.Not e -> do e <- nameTerm e; return $ S.Not e
  S.Neg e -> do e <- nameTerm e; return $ S.Neg e
  S.Add e1 e2 -> do e1 <- nameTerm e1; e2 <- nameTerm e2; return $ S.Add e1 e2
  S.Sub e1 e2 -> do e1 <- nameTerm e1; e2 <- nameTerm e2; return $ S.Sub e1 e2
  S.FNeg e -> do e <- nameTerm e; return $ S.FNeg e
  S.FAdd e1 e2 -> do e1 <- nameTerm e1; e2 <- nameTerm e2; return $ S.FAdd e1 e2
  S.FSub e1 e2 -> do e1 <- nameTerm e1; e2 <- nameTerm e2; return $ S.FSub e1 e2
  S.FMul e1 e2 -> do e1 <- nameTerm e1; e2 <- nameTerm e2; return $ S.FMul e1 e2
  S.FDiv e1 e2 -> do e1 <- nameTerm e1; e2 <- nameTerm e2; return $ S.FDiv e1 e2
  S.Eq e1 e2 -> do e1 <- nameTerm e1; e2 <- nameTerm e2; return $ S.Eq e1 e2
  S.LE e1 e2 -> do e1 <- nameTerm e1; e2 <- nameTerm e2; return $ S.LE e1 e2
  S.If e1 e2 e3 -> do e1 <- nameTerm e1; e2 <- nameTerm e2; e3 <- nameTerm e3; return $ S.If e1 e2 e3
  S.Let x t e1 e2 -> do
    x <- nameIdent x
    t <- nameType t
    e1 <- nameTerm e1
    e2 <- nameTerm e2
    return $ S.Let x t e1 e2
  S.Var x -> do x <- nameIdent x; return $ S.Var x
  S.LetRec x t ys e1 e2 -> do
    t <- nameType t
    ys <- for ys $ \(x, t) -> do
      x <- nameIdent x
      t <- nameType t
      return (x, t)
    e1 <- nameTerm e1
    e2 <- nameTerm e2
    return $ S.LetRec x t ys e1 e2
  S.App e es -> do e <- nameTerm e; es <- traverse nameTerm es; return $ S.App e es
  S.Tuple es -> do es <- traverse nameTerm es; return $ S.Tuple es
  S.LetTuple xs e1 e2 -> do
    xs <- for xs $ \(x, t) -> do
      x <- nameIdent x
      t <- nameType t
      return (x, t)
    e1 <- nameTerm e1
    e2 <- nameTerm e2
    return $ S.LetTuple xs e1 e2
  S.Array e1 e2 -> do e1 <- nameTerm e1; e2 <- nameTerm e2; return $ S.Array e1 e2
  S.Get e1 e2 -> do e1 <- nameTerm e1; e2 <- nameTerm e2; return $ S.Get e1 e2
  S.Put e1 e2 e3 -> do e1 <- nameTerm e1; e2 <- nameTerm e2; e3 <- nameTerm e3; return $ S.Put e1 e2 e3
  e -> return e
