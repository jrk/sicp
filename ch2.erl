-module(sicp02).
-export([tryme/0]).

print(X) ->
   io:write(X),
   io:format("~n").

tryme() ->
   section_2_1_1().

% Functions defined in previous chapters %
gcd(A, 0) -> A;
gcd(A, B) -> gcd(B, A rem B).

fib(0) -> 0;
fib(1) -> 1;
fib(N) -> fib(N - 1) + fib(N - 2).

identity(X) -> X.

% 2 Building Abstractions with Data
linear_combination(A, B, X, Y) ->
   (A * X) + (B * Y).

mul(A, B) -> A * B.

linear_combination_1(A, B, X, Y) ->
   mul(A, X) + mul(B, Y).

% 2.1.1 Introduction to Data Abstraction - Example: Arithmetic Operations for Rational Numbers
car([H|_]) -> H.
cdr([_|T]) -> T.
cadr(X) -> car(cdr(X)).
cons(X, Y) -> [X, Y].

% Literal Translation %
make_rat(N, D) -> [N, D].
numer(X) -> hd(X).
denom(X) -> hd(tl(X)).

add_rat(X, Y) ->
   make_rat((numer(X) * denom(Y)) + (numer(Y) * denom(X)), denom(X) * denom(Y)).
sub_rat(X, Y) ->
   make_rat((numer(X) * denom(Y)) - (numer(Y) * denom(X)), denom(X) * denom(Y)).
mul_rat(X, Y) ->
   make_rat(numer(X) * numer(Y), denom(X) * denom(Y)).
div_rat(X, Y) ->
   make_rat(numer(X) * denom(Y), denom(X) * numer(Y)).
equal_rat(X, Y) ->
   ((numer(X) * denom(Y)) == (numer(Y) * denom(X))).

print_rat(X) ->
   io:write(numer(X)),
   io:format('/'),
   io:write(denom(X)),
   io:format("~n").

% reducing to lowest terms in constructor %
make_rat_1(N, D) ->
   G = gcd(N, D),
   [N div G, D div G].

add_rat_1 (X, Y) ->
   make_rat_1((numer(X) * denom(Y)) + (numer(Y) * denom(X)), denom(X) * denom(Y)).
% end Literal Translation %

% Message Translation %

% end Message Translation %

section_2_1_1() ->
   X = cons(1, 2),
   Y = cons(3, 4),
   Z = cons(X, Y),
   print (car(car(Z))),
   print (car(cdr(Z))),

   One_Half = make_rat(1, 2),
   print_rat(One_Half),

   One_Third = make_rat(1, 3),
   print_rat(add_rat(One_Half, One_Third)),
   print_rat(mul_rat(One_Half, One_Third)),
   print_rat(add_rat(One_Third, One_Third)),

   print_rat(add_rat_1(One_Third, One_Third)),

   io:format("~n").
