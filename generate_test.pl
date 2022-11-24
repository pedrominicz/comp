:- ensure_loaded(library(lists)).

:- set_prolog_flag(optimise, true).
:- set_prolog_flag(optimise_unify, true).

% Based on A Hiking Trip Through the Orders of Magnitude: Deriving Efficient
% Generators for Closed Simply-Typed Lambda Terms and Normal Forms.
%
% https://arxiv.org/pdf/1608.03912.pdf

n2s(0, z) :- !.
n2s(N, s(X)) :- N > 0, N0 is N - 1, n2s(N0, X).

down(s(X), X).

% Generate simply-typed lambda calculus terms of a given size.
lambda(N, X) :- n2s(N, S), lambda(X, _, [], S, z).

lambda(v(X), A, Ctx) -->
  { member(X : A0, Ctx), unify_with_occurs_check(A0, A) }.
lambda(l(X, Y), A -> B, Ctx) -->
  down,
  lambda(Y, B, [X : A | Ctx]).
lambda(a(X, Y), B, Ctx) -->
  down,
  lambda(X, A -> B, Ctx),
  lambda(Y, A, Ctx).
lambda(let(X, Y1, Y2), B, Ctx) -->
  down,
  lambda(Y1, A, Ctx),
  lambda(Y2, B, [X : A | Ctx]).

% Generate typable SK-combinator calculus terms of a given size.
sk(N, X) :- n2s(N, S), sk(X, _, S, z).

sk(s, (A -> B -> C) -> (A -> B) -> A -> C) --> [].
sk(k, A -> _B -> A) --> [].
sk(a(X, Y), B) -->
  down,
  sk(X, A -> B),
  sk(Y, A0),
  { unify_with_occurs_check(A0, A) }.
sk(let(X : A, Y1, Y2), B, Ctx) -->
  down,
  sk(Y1, A, Ctx),
  sk(Y2, B, [X : A | Ctx]).

% Pretty print a term.
pretty(X) :-
  numbervars(X, 0, _),
  pretty(X, Xs, []),
  maplist(write, Xs),
  nl.

pretty(s) --> ['(\\x,\\y,\\z,x z(y z))'].
pretty(k) --> ['(\\x,\\y,x)'].
pretty(v('$VAR'(I))) --> [x, I].
pretty(l('$VAR'(I), X)) --> ['(\\x', I, ','], pretty(X), [')'].
pretty(a(X, Y)) --> ['('], pretty(X), [' '], pretty(Y), [')'].
pretty(let('$VAR'(I), Y1, Y2)) -->
  ['(let x', I, '='], pretty(Y1), [' in '], pretty(Y2), [')'].

% Print all simply-typed lambda calculus terms of a given size.
show_lambda(N) :-
  lambda(N, X),
  pretty(X),
  fail.
show_lambda(_).

% Print all typable SK-combinator calculus terms of a given size.
show_sk(N) :-
  sk(N, X),
  pretty(X),
  fail.
show_sk(_).

main :-
  between(1, 7, N),
  show_lambda(N),
  fail.
main :-
  between(0, 6, N),
  show_sk(N),
  fail.
main.
