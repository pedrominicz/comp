module KNormal where

import Gen
import Typing
import qualified Syntax as S
import qualified Type as T

import Control.Monad.State
import qualified Data.Map as M
import qualified Data.Set as S

data KNormal
  = Unit
  | Int Int
  | Float Float
  | Neg String
  | Add String String
  | Sub String String
  | FNeg String
  | FAdd String String
  | FSub String String
  | FMul String String
  | FDiv String String
  | IfEq String String KNormal KNormal
  | IfLE String String KNormal KNormal
  | Let String KNormal KNormal
  | Var String
  | LetRec String [String] KNormal KNormal
  | App String [String]
  | Tuple [String]
  | LetTuple [String] String KNormal
  | Get String String
  | Put String String String
  | ExtArray String
  | ExtFunApp String [String]
  deriving (Eq, Read, Show)

fv :: KNormal -> S.Set String
fv e = case e of
  Unit -> S.empty
  Int _ -> S.empty
  Float _ -> S.empty
  Neg x -> S.singleton x
  Add x y -> S.fromList [x, y]
  Sub x y -> S.fromList [x, y]
  FNeg x -> S.singleton x
  FAdd x y -> S.fromList [x, y]
  FSub x y -> S.fromList [x, y]
  FMul x y -> S.fromList [x, y]
  FDiv x y -> S.fromList [x, y]
  IfEq x y e1 e2 -> S.fromList [x, y] `S.union` fv e1 `S.union` fv e2
  IfLE x y e1 e2 -> S.fromList [x, y] `S.union` fv e1 `S.union` fv e2
  Let x e1 e2 -> fv e1 `S.union` S.delete x (fv e2)
  Var x -> S.singleton x
  LetRec x ys e1 e2 -> S.delete x $ (fv e1 S.\\ S.fromList ys) `S.union` fv e2
  App x ys -> S.fromList (x:ys)
  Tuple xs -> S.fromList xs
  LetTuple xs y e -> S.insert y $ fv e S.\\ S.fromList xs
  Get x y -> S.fromList [x, y]
  Put x y z -> S.fromList [x, y, z]
  ExtArray _ -> S.empty
  ExtFunApp _ ys -> S.fromList ys

insertLet :: (MonadTrans t, Monad m, Monad (t (GenT m))) => KNormal -> (String -> t (GenT m) KNormal) -> t (GenT m) KNormal
insertLet (Var x) k = k x
insertLet e k = do
  x <- show <$> lift gen
  e' <- k x
  return $ Let x e e'

convert :: S.Set String -> S.Syntax -> Env -> GenT Maybe KNormal
convert bound e env = evalStateT (go bound e) env
  where
  go :: S.Set String -> S.Syntax -> StateT Env (GenT Maybe) KNormal
  go bound e = case e of
    S.Unit -> return Unit
    S.Bool bool -> return . Int $ if bool then 1 else 0
    S.Int int -> return $ Int int
    S.Float float -> return $ Float float
    S.Not e -> go bound $ S.If e (S.Bool False) (S.Bool True)
    S.Neg e -> do
      e <- go bound e
      insertLet e $ return . Neg
    S.Add e1 e2 -> do
      e1 <- go bound e1
      insertLet e1 $ \x -> do
        e2 <- go bound e2
        insertLet e2 $ \y -> return $ Add x y
    S.Sub e1 e2 -> do
      e1 <- go bound e1
      insertLet e1 $ \x -> do
        e2 <- go bound e2
        insertLet e2 $ \y -> return $ Sub x y
    S.FNeg e -> do
      e <- go bound e
      insertLet e $ return . FNeg
    S.FAdd e1 e2 -> do
      e1 <- go bound e1
      insertLet e1 $ \x -> do
        e2 <- go bound e2
        insertLet e2 $ \y -> return $ FAdd x y
    S.FSub e1 e2 -> do
      e1 <- go bound e1
      insertLet e1 $ \x -> do
        e2 <- go bound e2
        insertLet e2 $ \y -> return $ FSub x y
    S.FMul e1 e2 -> do
      e1 <- go bound e1
      insertLet e1 $ \x -> do
        e2 <- go bound e2
        insertLet e2 $ \y -> return $ FMul x y
    S.FDiv e1 e2 -> do
      e1 <- go bound e1
      insertLet e1 $ \x -> do
        e2 <- go bound e2
        insertLet e2 $ \y -> return $ FDiv x y
    S.Eq _ _ -> go bound $ S.If e (S.Bool True) (S.Bool False)
    S.LE _ _ -> go bound $ S.If e (S.Bool True) (S.Bool False)
    S.If (S.Not e1) e2 e3 -> go bound $ S.If e1 e3 e2
    S.If (S.Eq e1 e2) e3 e4 -> do
      e1 <- go bound e1
      insertLet e1 $ \x -> do
        e2 <- go bound e2
        insertLet e2 $ \y -> do
          e3 <- go bound e3
          e4 <- go bound e4
          return $ IfEq x y e3 e4
    S.If (S.LE e1 e2) e3 e4 -> do
      e1 <- go bound e1
      insertLet e1 $ \x -> do
        e2 <- go bound e2
        insertLet e2 $ \y -> do
          e3 <- go bound e3
          e4 <- go bound e4
          return $ IfLE x y e3 e4
    S.If e1 e2 e3 -> go bound $ S.If (S.Eq e1 (S.Bool False)) e3 e2
    S.Let x _ e1 e2 -> do
      e1 <- go bound e1
      e2 <- go (S.insert x bound) e2
      return $ Let x e1 e2
    S.Var x | x `S.member` bound -> return $ Var x
    S.Var x -> do
      env <- get
      case env M.! x of
        T.Array _ -> return $ ExtArray x
        _ -> mzero
    S.LetRec x _ ys' e1 e2 -> do
      let ys = map fst ys'
      let bound' = S.insert x bound
      e1 <- go (bound' `S.union` S.fromList ys) e1
      e2 <- go bound' e2
      return $ LetRec x ys e1 e2
    S.App (S.Var f) es | not (f `S.member` bound) -> do
      let bind xs [] = return $ ExtFunApp f xs
          bind xs (e:es) = do
            e <- go bound e
            insertLet e $ \x ->
              bind (xs ++ [x]) es
      bind [] es
    S.App f es -> do
      f <- go bound f
      insertLet f $ \f -> do
        let bind xs [] = return $ App f xs
            bind xs (e:es) = do
              e <- go bound e
              insertLet e $ \x ->
                bind (xs ++ [x]) es
        bind [] es
    S.Tuple es -> do
      let bind xs [] = return $ Tuple xs
          bind xs (e:es) = do
            e <- go bound e
            insertLet e $ \x ->
              bind (xs ++ [x]) es
      bind [] es
    S.LetTuple xs' e1 e2 -> do
      let xs = map fst xs'
      e1 <- go bound e1
      insertLet e1 $ \y -> do
        e2 <- go (bound `S.union` S.fromList xs) e2
        return $ LetTuple xs y e2
    S.Array e1 e2 -> do
      e1 <- go bound e1
      insertLet e1 $ \x -> do
        e2 <- go bound e2
        -- Since we are not carrying types around in this function, there is no
        -- way to know if this is an `int` or `float` array. So, we assume it
        -- is an array of integers.
        insertLet e2 $ \y -> return $ ExtFunApp "create_array" [x, y]
    S.Get e1 e2 -> do
      e1 <- go bound e1
      insertLet e1 $ \x -> do
        e2 <- go bound e2
        insertLet e2 $ \y -> return $ Get x y
    S.Put e1 e2 e3 -> do
      e1 <- go bound e1
      insertLet e1 $ \x -> do
        e2 <- go bound e2
        insertLet e2 $ \y -> do
          e3 <- go bound e3
          insertLet e3 $ \z -> return $ Put x y z
