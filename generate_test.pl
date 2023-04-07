:- ensure_loaded(library(lists)).

n2s(0, z) :- !.
n2s(N, s(X)) :- N > 0, N0 is N - 1, n2s(N0, X).

down(s(X), X).

down2(s(s(X)), X).

% Generate closed lambda terms of a given size.
lambda(N, X) :- n2s(N, S), lambda(X, [], S, z).

lambda(v(X), Ctx) --> { member(X, Ctx) }.
lambda(l(X, Y), Ctx) --> down, lambda(Y, [X|Ctx]).
lambda(a(X, Y), Ctx) --> down, lambda(X, Ctx), lambda(Y, Ctx).
%lambda(let(X, Y, Z), Ctx) --> down2, lambda(Y, Ctx), lambda(Z, [X|Ctx]).

% Infer the type of a closed lambda term.
infer(X, T) :- infer(X, T, []).

infer(v(X), A, Ctx) :-
  member(X : A0, Ctx),
  unify_with_occurs_check(A0, A).
infer(l(X, Y), A -> B, Ctx) :- infer(Y, B, [X : A | Ctx]).
infer(a(X, Y), B, Ctx) :- infer(X, A -> B, Ctx), infer(Y, A, Ctx).
%infer(let(X, Y, Z), B, Ctx) :- infer(Y, A, Ctx), infer(Z, B, [X : A | Ctx]).

% Pretty print a lambda term.
pretty(X) :-
  numbervars(X, 0, _),
  pretty(X, Xs, []),
  maplist(write, Xs),
  nl,
  pretty_type(X).

pretty(v('$VAR'(I))) --> [x, I].
pretty(l('$VAR'(I), X)) --> ['(λ ', x, I, ', '], pretty(X), [')'].
pretty(a(X, Y)) --> ['('], pretty(X), [' '], pretty(Y), [')'].
%pretty(let('$VAR'(I), Y, Z)) -->
%  ['(let ', x, I, ' = '], pretty(Y), [' in '], pretty(Z), [')'].

pretty_type(X) :-
  infer(X, A), !,
  numbervars(A, 0, _),
  pretty_type(0, A, As, []),
  maplist(write, As),
  nl.
pretty_type(_) :- writeln('?').

pretty_type(0, A -> B) --> pretty_type(1, A), !, [' → '], pretty_type(0, B).
pretty_type(1, A -> B) -->
  ['('], pretty_type(1, A), !, [' → '], pretty_type(0, B), [')'].
pretty_type(_, A) --> [A].

% Print all simply-typed lambda terms of a given size.
show(N) :-
  lambda(N, X),
  pretty(X),
  fail.
show(_).

main :-
  between(1, 8, N),
  show(N),
  fail.
main.
