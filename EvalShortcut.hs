{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module EvalShortcut (eval) where

import Expr

import Data.Kind
import Unsafe.Coerce

type Level = Int

data ClosureF :: Bool -> Type where
  Closure :: ExprF 'True -> Int -> Env -> ClosureF b
  Level   :: Level -> ClosureF 'True
  Expr    :: ExprF 'False -> ClosureF 'False

type Closure = ClosureF 'True

type Env = [Closure]

compile :: ExprF 'False -> ExprF 'True
compile = unsafeCoerce

-- Preponed reduction-free shortcut normalizer (`normalise16`) based on the
-- paper Deriving the Full-Reducing Krivine Machine from the Small-Step
-- Operational Semantics of Normal Order.
--
-- Two times slower than `Eval.eval`.
eval :: Expr -> Expr
eval e = snd $ nf 0 (Closure (compile e) 0 [])
  where
  nf :: Level -> Closure -> (Level, Expr)
  nf l (Closure e len env) =
    case e of
      Var x ->
        if x < len
          then nf l (env !! x)
          else (l, Var (x - (len - l)))
      Lam b ->
        let (l', b') = nf (l + 1) (Closure b (len + 1) (Level (l + 1) : env)) in
        (l' - 1, Lam b')
      App f' a' ->
        let f = Closure f' len env
            a = Closure a' len env
            (l', c) = whnf l f in
        case c of
          Closure b len env -> nf l' (Closure b len (a : tail env))
          Expr e -> App e <$> nf l' a
  nf l (Level l') = (l, Var (l - l'))

  whnf :: Level -> Closure -> (Level, ClosureF 'False)
  whnf l (Closure e len env) =
    case e of
      Var x ->
        if x < len
          then whnf l (env !! x)
          else (l, Expr (Var (x - (len - l))))
      Lam b -> (l, Closure b (len + 1) (Level (l + 1) : env))
      App f' a' ->
        let f = Closure f' len env
            a = Closure a' len env
            (l', c) = whnf l f in
        case c of
          Closure b len env -> whnf l' (Closure b len (a : tail env))
          Expr e -> Expr . App e <$> nf l' a
  whnf l (Level l') = (l, Expr (Var (l - l')))
