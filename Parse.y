{
module Parse (parse) where

import Syntax
import qualified Lex as L

import Data.ByteString (ByteString)
}

%expect 0

%tokentype { L.Token }

%monad { Maybe }
%error { const Nothing }

%name expr expr

%token
  var           { L.Var $$ }
  '='           { L.Equal }
  ','           { L.Comma }
  '('           { L.LParen }
  ')'           { L.RParen }
  'λ'           { L.Lam }
  'let'         { L.Let }
  'in'          { L.In }
%%

-- Expressions

expr :: { Expr }
  : 'λ' arguments ',' expr        { foldr Lam $4 $2 }
  | 'let' var '=' expr 'in' expr  { Let $2 $4 $6 }
  | application                   { $1 }

application :: { Expr }
  : application simple            { App $1 $2 }
  | simple                        { $1 }

simple :: { Expr }
  : var                           { Var $1 }
  | '(' expr ')'                  { $2 }

arguments :: { [ByteString] }
  : var                           { [$1] }
  | var arguments                 { $1 : $2 }

{
parse :: ByteString -> Maybe Expr
parse str = L.lexer str >>= expr
}
