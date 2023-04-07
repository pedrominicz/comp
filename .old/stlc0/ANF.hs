module ANF (normalize) where

import Syntax
import qualified Expr as E

-- XXX BENCHMARK STRICT STATE
import Control.Monad.State
import Data.List (elemIndex)
import qualified Data.ByteString as B

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

fresh :: State Int Name
fresh = state $ \x -> ((B.empty, x), x + 1)

nameless :: Expr -> Maybe E.Expr
nameless = go []
  where
  go :: [Name] -> Expr -> Maybe E.Expr
  go ctx (Var x) = E.Var <$> elemIndex x ctx
  go ctx (Lam x b) = E.Lam <$> go (x : ctx) b
  go ctx (App (Var f) (Var a)) =
    E.App <$> elemIndex f ctx <*> elemIndex a ctx
  go ctx (Let x e1 e2) = E.Let <$> go ctx e1 <*> go (x : ctx) e2
  go _ (Num n) = Just (E.Num n)
  go ctx (Add (Var x1) (Var x2)) =
    E.Add <$> elemIndex x1 ctx <*> elemIndex x2 ctx
  go _ _ = error "unreachable"

normalize :: Expr -> Maybe E.Expr
normalize e = nameless $ evalState (go e) 0
  where
  go :: Expr -> State Int Expr
  go (Var x) = return $ Var x
  go (Lam x b) = Lam x <$> go b
  go (App f a) = do
    f <- go f
    a <- go a
    name f $ \f -> name a $ \a -> return $ App (Var f) (Var a)
  go (Let x e1 e2) = do
    e1 <- go e1
    e2 <- go e2
    -- As pointed out in Compiling with Continuations, Continued, the
    -- A-normalization algorithm in The Essence of Compiling with Continuations
    -- doesn't actually normalizes terms, as it doesn't deal with the let
    -- expression invariant.
    case e1 of
      Let x' e1' e2' -> return $ Let x' e1' (Let x e2' e2)
      _ -> return $ Let x e1 e2
  go (Num n) = return $ Num n
  go (Add e1 e2) = do
    e1 <- go e1
    e2 <- go e2
    name e1 $ \x1 -> name e2 $ \x2 -> return $ Add (Var x1) (Var x2)

  name :: Expr -> (Name -> State Int Expr) -> State Int Expr
  name (Var x) k = k x
  name e k = do
    x <- fresh
    Let x e <$> k x
