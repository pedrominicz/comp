module Pretty (pretty) where

import Expr

pretty :: Expr -> String
pretty = expr 0
  where
  expr :: Int -> Expr -> String
  expr k (Lam b) = "λ " ++ vars !! k ++ ", " ++ expr (k + 1) b
  expr k e = application k e

  application :: Int -> Expr -> String
  application k (App f a) = application k f ++ " " ++ simple k a
  application k e = simple k e

  simple :: Int -> Expr -> String
  simple k (Var x) = vars !! (k - x - 1)
  simple k e = "(" ++ expr k e ++ ")"

  -- May explode.
  vars :: [String]
  vars = map return ['a'..'z']