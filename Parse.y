{
module Parse (parse) where

import qualified Expr as E
import qualified Lex as L

import Data.ByteString (ByteString)
import Data.List
}

%expect 0

%tokentype { L.Token }

%monad { Maybe }
%error { const Nothing }

%name expr expr

%token
  var           { L.Var $$ }
  'λ'           { L.Lam }
  ','           { L.Comma }
  '('           { L.LParen }
  ')'           { L.RParen }
%%

-- Expressions

expr :: { Expr }
  : 'λ' arguments ',' expr      { foldr Lam $4 $2 }
  | application                 { $1 }

application :: { Expr }
  : application simple          { App $1 $2 }
  | simple                      { $1 }

simple :: { Expr }
  : var                         { Var $1 }
  | '(' expr ')'                { $2 }

arguments :: { [ByteString] }
  : var                         { [$1] }
  | var arguments               { $1 : $2 }

{
data Expr
  = Var ByteString
  | Lam ByteString Expr
  | App Expr Expr

nameless :: Expr -> Maybe E.Expr
nameless = go []
  where
  go :: [ByteString] -> Expr -> Maybe E.Expr
  go ctx (Var x) = E.Var <$> elemIndex x ctx
  go ctx (Lam x b) = E.Lam <$> go (x : ctx) b
  go ctx (App f a) = E.App <$> go ctx f <*> go ctx a

parse :: ByteString -> Maybe E.Expr
parse str = L.lexer str >>= expr >>= nameless
}
