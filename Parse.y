{
module Parse where

import Lex (Token)
import qualified Lex as L
import Syntax

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
}

%tokentype { Token }

%monad { Maybe } { (>>=) } { return }
%error { const Nothing }

%name parseExpr expr
%name parseStmt stmt

%token
  -- Keywords
  'else'        { L.Else }
  'if'          { L.If }
  'int'         { L.Int }
  'return'      { L.Return }
  'while'       { L.While }
  -- Identifiers
  identifier    { L.Identifier $$ }
  -- Literals
  integer       { L.IntLiteral $$ }
  string        { L.StringLiteral $$ }
  -- Operators
  '<<'          { L.ShiftLeft }
  '>>'          { L.ShiftRight }
  '<='          { L.LE }
  '>='          { L.GE }
  '=='          { L.EQ' } -- Avoids conflict with `Prelude.EQ`.
  '!='          { L.NE }
  ';'           { L.Semicolon }
  '{'           { L.OpenBrace }
  '}'           { L.CloseBrace }
  ','           { L.Comma }
  '='           { L.Equals }
  '('           { L.OpenParen }
  ')'           { L.CloseParen }
  '&'           { L.BitwiseAnd }
  '!'           { L.Not }
  '~'           { L.BitwiseNot }
  '-'           { L.Minus }
  '+'           { L.Plus }
  '*'           { L.Asterisk }
  '/'           { L.Div }
  '%'           { L.Mod }
  '<'           { L.LT' } -- Avoids conflict with `Prelude.LT`.
  '>'           { L.GT' } -- Avoids conflict with `Prelude.GT`.
  '^'           { L.BitwiseXor }
  '|'           { L.BitwiseOr }
%%

-----------------
-- Expressions --
-----------------

prim_expr :: { Expr }
  : identifier                  { Identifier $1 }
  | integer                     { IntLiteral $1 }
  | string                      { StringLiteral $1 }
  | '(' expr ')'                { $2 }

postfix_expr :: { Expr }
  : prim_expr                   { $1 }
  | postfix_expr '(' ')'        { FunctionCall $1 [] }
  | postfix_expr '(' expr_list ')' { FunctionCall $1 $3 }

expr_list :: { [Expr] }
  : expr                        { [$1] }
  | expr ',' expr_list          { $1 : $3 }

unary_expr :: { Expr }
  : postfix_expr                { $1 }
  | '&' unary_expr              { AddressOf $2 }
  | '*' unary_expr              { Indirection $2 }
  | '+' unary_expr              { $2 }
  | '-' unary_expr              { Negative $2 }
  | '~' unary_expr              { BitwiseNot $2 }
  | '!' unary_expr              { Not $2 }

mul_expr :: { Expr }
  : unary_expr                  { $1 }
  | mul_expr '*' unary_expr     { Mul $1 $3 }
  | mul_expr '/' unary_expr     { Div $1 $3 }
  | mul_expr '%' unary_expr     { Mod $1 $3 }

add_expr :: { Expr }
  : mul_expr                    { $1 }
  | add_expr '+' mul_expr       { Add $1 $3 }
  | add_expr '-' mul_expr       { Sub $1 $3 }

shift_expr :: { Expr }
  : add_expr                    { $1 }
  | shift_expr '<<' add_expr    { ShiftLeft $1 $3 }
  | shift_expr '>>' add_expr    { ShiftRight $1 $3 }

rel_expr :: { Expr }
  : shift_expr                  { $1 }
  | rel_expr '<' shift_expr     { LT' $1 $3 }
  | rel_expr '>' shift_expr     { GT' $1 $3 }
  | rel_expr '<=' shift_expr    { LE $1 $3 }
  | rel_expr '>=' shift_expr    { GE $1 $3 }

eq_expr :: { Expr }
  : rel_expr                    { $1 }
  | eq_expr '==' rel_expr       { EQ' $1 $3 }
  | eq_expr '!=' rel_expr       { NE $1 $3 }

and_expr :: { Expr }
  : eq_expr                     { $1 }
  | and_expr '&' eq_expr        { BitwiseAnd $1 $3 }

xor_expr :: { Expr }
  : and_expr                    { $1 }
  | xor_expr '^' and_expr       { BitwiseXor $1 $3 }

or_expr :: { Expr }
  : xor_expr                    { $1 }
  | or_expr '|' xor_expr        { BitwiseOr $1 $3 }

expr :: { Expr }
  : or_expr                     { $1 }
  | identifier '=' expr         { Assignment $1 $3 }

----------------
-- Statements --
----------------

stmt :: { Stmt }
  : expr ';'                    { Expression $1 }
  | 'if' '(' expr ')' block     { If $3 $5 }
  | 'if' '(' expr ')' block 'else' block { IfElse $3 $5 $7 }
  | 'while' '(' expr ')' block  { While $3 $5 }
  | 'return' expr ';'           { Return $2 }

block :: { Stmt }
  : '{' stmt_list '}'           { Compound [] $2 }
  | '{' decl_list stmt_list '}' { Compound $2 $3 }

decl :: { ByteString }
  : 'int' identifier ';'        { $2 }

decl_list :: { [ByteString] }
  : decl                        { [$1] }
  | decl decl_list              { $1 : $2 }

stmt_list :: { [Stmt] }
  : stmt                        { [$1] }
  | stmt stmt_list              { $1 : $2 }
