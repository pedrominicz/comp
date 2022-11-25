module ANF (AExpr(..), CExpr(..), Expr(..), Name, normalize) where

import qualified Syntax as S

import Control.Monad.State
import Data.ByteString (ByteString, empty)

-- Administrative normal form (ANF) expressions have to satisfy the constraints
-- imposed by the two grammars below. Each grammar reveals some structure not
-- so apparent in the other.
--
-- Grammar based on The Essence of Compiling with Continuations:
--
--  <expr> ::= <value>
--           | 'let' <var> '=' <value> 'in' <expr>
--           | <value> <value>
--           | 'let' <var> '=' <value> <value> 'in' <expr>
--
--  <value> ::= <var>
--            | 'λ' <var> ',' <expr>
--
-- Let expressions can only bind variables, applications and primitive
-- operations (not present in the grammar above), in particular, let
-- expressions cannot be let bound.
--
-- Grammar based on `https://matt.might.net/articles/a-normalization/`:
--
--  <aexpr> ::= <var>
--            | 'λ' <var> ',' <expr>
--
--  <cexpr> ::= <aexpr> <aexpr>
--
--  <expr> ::= 'let' <var> '=' <expr> 'in' <expr>
--           | <aexpr>
--           | <cexpr>
--
-- Evaluating an atomic expression (`<aexpr>`) is guaranteed to terminate,
-- cause no side effects, cause no control effects and never produce an error.
-- A lambda expression is atomic because evaluating it creates a closure and
-- that process satisfies the required constraints.
--
-- Evaluating a complex expression (`<cexpr>`) may not terminate, may have a
-- side effect and may produce an error. However, a complex expression may
-- defer execution to only one other complex expression. For example, the
-- following are also complex expressions:
--
--  <cexpr> ::= <aexpr> <aexpr>
--            | 'if' <aexpr> 'then' <expr> 'else' <expr>
--            | 'let' 'rec' <var> '=' <aexpr> 'in' <expr>
--
-- Although function application can apply any two values, the normalizer below
-- only allows applying variables to variables. This is similar to the
-- continuation passing style (CPS) grammars in Compiling with Continuations,
-- Continued.

type Name = (ByteString, Int)

data AExpr
  = Var {-# UNPACK #-} !Name
  | Lam {-# UNPACK #-} !Name Expr
  deriving (Eq, Show)

data CExpr
  = App {-# UNPACK #-} !Name {-# UNPACK #-} !Name
  deriving (Eq, Show)

data Expr
  = AExpr AExpr
  | CExpr CExpr
  | Let {-# UNPACK #-} !Name Expr Expr
  deriving (Eq, Show)

fresh :: State Int Name
fresh = state $ \x -> ((empty, x), x + 1)

normalize :: S.Expr -> Expr
normalize e = evalState (expr e) 0
  where
  expr :: S.Expr -> State Int Expr
  expr (S.Var x) = return $ AExpr (Var (x, 0))
  expr (S.Lam x b) = AExpr . Lam (x, 0) <$> expr b
  expr (S.App f a) =
    name f $ \f ->
      name a $ \a ->
        return $ CExpr (App f a) 
  expr (S.Let x e1 e2) = do
    e1 <- expr e1
    e2 <- expr e2
    -- As pointed out in Compiling with Continuations, Continued, the
    -- A-normalization algorithm in The Essence of Compiling with Continuations
    -- doesn't actually normalizes terms, as it doesn't deal with the let
    -- expression invariant.
    case e1 of
      Let x' e1' e2' -> return $ Let x' e1' (Let (x, 0) e2' e2)
      _ -> return $ Let (x, 0) e1 e2

  name :: S.Expr -> (Name -> State Int Expr) -> State Int Expr
  name e k = do
    e <- expr e
    case e of
      AExpr (Var x) -> k x
      _ -> do
        x <- fresh
        Let x e <$> k x
