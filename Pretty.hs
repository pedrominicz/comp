module Pretty where

import Expr

import Data.Maybe

pretty :: Expr -> String
pretty e = fromMaybe (expr 0 e) (num e)
  where
  num :: Expr -> Maybe String
  num (Lam (Lam e)) = show <$> go e
    where
    go :: Expr -> Maybe Int
    go (Var 0) = Just 0
    go (App (Var 1) e) = succ <$> go e
    go _ = Nothing
  num _ = Nothing

  expr :: Int -> Expr -> String
  expr k (Lam b) = "λ " ++ vars !! k ++ ", " ++ expr (k + 1) b
  expr k e = app k e

  app :: Int -> Expr -> String
  app k (App f a) = app k f ++ " " ++ simple k a
  app k e = simple k e

  simple :: Int -> Expr -> String
  simple k (Var x) =
    let i = k - x - 1 in
    if i < 0 then show (-i - 1) else vars !! i
  simple k e = "(" ++ expr k e ++ ")"

  -- May explode.
  vars :: [String]
  vars = map return ['a'..'z']
