{
module Parse where

import Lex (Token)
import qualified Lex as L
import Syntax

import qualified Data.ByteString.Lazy as B
}

%tokentype { Token }

%monad { Maybe } { (>>=) } { return }
%error { const Nothing }

%name parseExpr expr
%name parseStmt stmt
%name parseFunc func
%name parseProg prog

%token
  -- Keywords
  'else'        { L.Else }
  'if'          { L.If }
  'int'         { L.Int }
  'return'      { L.Return }
  'while'       { L.While }
  -- Identifiers
  identifier    { L.Id $$ }
  -- Literals
  integer       { L.IntLiteral $$ }
  -- Operators
  ';'           { L.Semicolon }
  '{'           { L.OpenBrace }
  '}'           { L.CloseBrace }
  ','           { L.Comma }
  '='           { L.Equals }
  '('           { L.OpenParen }
  ')'           { L.CloseParen }
  '+'           { L.Plus }
%%

-----------------
-- Expressions --
-----------------

prim_expr :: { Expr }
  : identifier                  { Id $1 }
  | integer                     { Int $1 }
  | '(' expr ')'                { $2 }

postfix_expr :: { Expr }
  : prim_expr                   { $1 }
  | postfix_expr '(' ')'        { Call $1 [] }
  | postfix_expr '(' expr_list ')' { Call $1 $3 }

expr_list :: { [Expr] }
  : expr                        { [$1] }
  | expr ',' expr_list          { $1 : $3 }

add_expr :: { Expr }
  : postfix_expr                { $1 }
  | add_expr '+' postfix_expr   { Add $1 $3 }

expr :: { Expr }
  : add_expr                    { $1 }
  | identifier '=' expr         { Assign $1 $3 }

----------------
-- Statements --
----------------

stmt :: { Stmt }
  : expr ';'                    { Expr $1 }
  | 'if' '(' expr ')' block     { If $3 $5 }
  | 'if' '(' expr ')' block 'else' block { IfElse $3 $5 $7 }
  | 'while' '(' expr ')' block  { While $3 $5 }
  | 'return' expr ';'           { Return $2 }

block :: { [Stmt] }
  : '{' stmt_list '}'           { $2 }

stmt_list :: { [Stmt] }
  : stmt                        { [$1] }
  | stmt stmt_list              { $1 : $2 }

---------------
-- Functions --
---------------

arg :: { B.ByteString }
  : 'int' identifier            { $2 }

arg_list :: { [B.ByteString] }
  : arg                         { [$1] }
  | arg ',' arg_list            { $1 : $3 }

decl :: { B.ByteString }
  : 'int' identifier ';'        { $2 }

decl_list :: { [B.ByteString] }
  : decl                        { [$1] }
  | decl decl_list              { $1 : $2 }

body :: { ([B.ByteString], [Stmt]) }
  : '{' decl_list stmt_list '}' { ($2, $3) }
  | '{' stmt_list '}'           { ([], $2) }

func :: { Func }
  : 'int' identifier '(' ')' body { Func $2 [] (fst $5) (snd $5) }
  | 'int' identifier '(' arg_list ')' body { Func $2 $4 (fst $6) (snd $6) }

-------------
-- Program --
-------------

func_list :: { [Func] }
  : func                        { [$1] }
  | func func_list              { $1 : $2 }

prog :: { [Func] }
  : func_list                   { $1 }
