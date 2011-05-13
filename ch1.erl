-module(sicp01).
-export([tryme/0]).

print(X) ->
   io:write(X),
   io:format("~n").

tryme() ->
   section_1_1_1(),
   section_1_1_2(),
   section_1_1_3(),
   section_1_1_4(),
   section_1_1_5(),
   section_1_1_6(),
   section_1_1_7(),
   section_1_1_8(),
   section_1_2_1(),
   section_1_2_2(),
   section_1_2_3(),
   section_1_2_4(),
   section_1_2_5(),
   section_1_2_6(),
   section_1_3_1(),
   section_1_3_2(),
   section_1_3_3(),
   section_1_3_4().


% 1.1.1 The Elements of Programming - Expressions
section_1_1_1() ->
   print (486),
   print (137 + 349),
   print (1000 - 334),
   print (5 * 99),
   print (10 div 5),
   print (2.7 + 10),
   print (21 + 35 + 12 + 7),
   print (25 * 4 * 12),
   print ((3 * 5) + (10 - 6)),
   print ((3 * ((2 * 4) + (3 + 5))) + ((10 - 7) + 6)).


% 1.1.2 The Elements of Programming - Naming and the Environment
section_1_1_2() ->
   Size = 2,
   print (Size),
   5 * Size,
   Pi = 3.14159,
   Radius = 10,
   print (Pi * Radius * Radius),
   Circumference = 2 * Pi * Radius,
   print (Circumference).


% 1.1.3 The Elements of Programming - Evaluating Combinations
section_1_1_3() ->
   print ((2 + (4 * 6)) * (3 + 5 + 7)).


% 1.1.4 The Elements of Programming - Compound Procedures
square(X) -> X * X.

sum_of_squares(X, Y) -> square(X) + square(Y).

f(A) -> sum_of_squares(A + 1, A * 2).

section_1_1_4() ->
   print (square(21)),
   print (square(2 + 5)),
   print (square(square(3))),
   sum_of_squares(3, 4),
   print (f(5)).


% 1.1.5 The Elements of Programming - The Substitution Model for Procedure Application
section_1_1_5() ->
   print (f(5)),
   print (sum_of_squares(5 + 1, 5 * 2)),
   print (square(6) + square(10)),
   print ((6 * 6) + (10 * 10)),
   print (36 + 100),
   print (f(5)),
   print (sum_of_squares(5 + 1, 5 * 2)),
   print (square(5 + 1) + square(5 * 2)),

   print (((5 + 1) * (5 + 1)) + ((5 * 2) * (5 * 2))),
   print ((6 * 6) + (10 * 10)),
   print (36 + 100),
   print (136).


% 1.1.6 The Elements of Programming - Conditional Expressions and Predicates
abs_0(X) ->
   if
      (X > 0) -> X;
      true ->
         if
            (X == 0) -> 0;
            true -> -X
         end
   end.

abs_1(X) ->
   if
      (X > 0) -> X;
      (X == 0) -> 0;
      true -> -X
   end.

abs_2(X) ->
   if
      (X < 0) -> -X;
      true -> X
   end.

ge(X, Y) -> (X > Y) or (X == Y).

ge_1(X, Y) -> not(X < Y).

% Exercise 1.4 %
a_plus_abs_b(A, B) ->
   if
      (B > 0) -> A + B;
      true -> A - B
   end.

% Exercise 1.5 %
p() -> p().
test(X, Y) ->
   if
      (X == 0) -> 0;
      true -> Y
   end.

section_1_1_6() ->
   X = 666,
   print ((X > 5) and (X < 10)),

   % Exercise 1.1 %
   print (10),
   print (5 + 3 + 4),
   print (9 - 1),
   print (6 div 2),
   print ((2 * 4) + (4 - 6)),
   A = 3,
   B = A + 1,
   print (A + B + (A * B)),
   print (A == B),
   print (if ((B > A) and (B < (A * B))) -> B; true -> A end),
   print (
      if
         (A == 4) -> 6;
         true ->
            if
               (B == 4) -> 6 + 7 + A;
               true -> 25
            end
      end),
   print (2 + (if (B > A) -> B; true -> A end)),
   print (
      (if
         (A > B) -> A;
         (A < B) -> B;
         true -> -1
       end) * (A + 1)),

   % Exercise 1.2 %
   print ((5 + 4 + (2 - (3 - (6 + (4 / 5))))) /
            (3 * (6 - 2) * (2 - 7))).

   % Exercise 1.5 %
   % commented out as this is in infinite loop
   % test(0, p()).


% 1.1.7 The Elements of Programming - Example: Square Roots by Newton's Method
good_enough(Guess, X) ->
   abs_0(square(Guess) - X) < 0.001.

average(X, Y) ->
   (X + Y) / 2.

improve(Guess, X) ->
   average(Guess, X / Guess).

sqrt_iter(Guess, X) ->
   case good_enough(Guess, X) of
      true -> Guess;
      false -> sqrt_iter(improve(Guess, X), X)
   end.

sqrt(X) ->
   sqrt_iter(1, X).

% Exercise 1.6 %
new_if(Predicate, Then_Clause, Else_Clause) ->
   case (Predicate) of
      true -> Then_Clause;
      false -> Else_Clause
   end.

sqrt_iter_0(Guess, X) ->
   new_if(
      good_enough(Guess, X),
      Guess,
      sqrt_iter(improve(Guess, X), X)).

% from wadler paper %
newif_1(true, X, _) -> X;
newif_1(false, _, Y) -> Y.

section_1_1_7() ->
   print (sqrt(9)),
   print (sqrt(100 + 37)),
   print (sqrt(sqrt(2) + sqrt(3))),
   print (square(sqrt(1000))),

   % Exercise 1.6 %
   print (new_if((2==3), 0, 5)),
   print (new_if((1==1), 0, 5)).


% 1.1.8 The Elements of Programming - Procedures as Black-Box Abstractions

% same as above
% square(X) -> X * X.

double(X) -> X + X.

square_2(X) -> math:exp(double(math:log(X))).

good_enough_1(Guess, X) ->
   abs_1(square(Guess) - X) < 0.001.

improve_1(Guess, X) ->
   average(Guess, X / Guess).

sqrt_iter_1(Guess, X) ->
   case (good_enough_1(Guess, X)) of
      true -> Guess;
      false -> sqrt_iter_1(improve_1(Guess, X), X)
   end.

sqrt_1(X) ->
   sqrt_iter_1(1.0, X).

% Block-structured %
sqrt_2(X) ->
   Good_Enough =
      fun (Guess, X) ->
         abs(square(Guess) - X) < 0.001
      end,
   Improve =
      fun (Guess, X) ->
         average(Guess, X / Guess)
      end,
   Sqrt_Iter =
      fun (Self, Guess, X) ->
         case (Good_Enough(Guess, X)) of
            true -> Guess;
            false -> Self(Self, Improve(Guess, X), X)
         end
      end,
   Sqrt_Iter(Sqrt_Iter, 1.0, X).


% Taking advantage of lexical scoping %
sqrt_3(X) ->
   Good_Enough =
      fun (Guess) ->
         abs(square(Guess) - X) < 0.001
      end,
   Improve =
      fun (Guess) ->
         average(Guess, X / Guess)
      end,
   Sqrt_Iter =
      fun (Self, Guess) ->
         case (Good_Enough(Guess)) of
            true -> Guess;
            false -> Self(Self, Improve(Guess))
         end
      end,
   Sqrt_Iter(Sqrt_Iter, 1.0).

section_1_1_8() ->
   print (square(5)),
   print (sqrt_2(25)),
   print (sqrt_3(25)),
   print (56789).


% 1.2.1 Procedures and the Processes They Generate - Linear Recursion and Iteration

% Recursive %
factorial(N) ->
   case (N == 1) of
      true -> 1;
      false -> N * factorial(N - 1)
   end.

% Iterative %
fact_iter_1(Product, Counter, Max_Count) ->
   case (Counter > Max_Count) of
      true -> Product;
      false -> fact_iter_1(Counter * Product, Counter + 1, Max_Count)
   end.

factorial_1(N) ->
   fact_iter_1(1, 1, N).

% Iterative, block-structured (from footnote) %
factorial_2(N) ->
   Iter =
      fun (Self, Product, Counter) ->
         case (Counter > N) of
            true -> Product;
            false -> Self(Self, Counter * Product, Counter + 1)
         end
      end,
   Iter(Iter, 1, 1).

% Exercise 1.9 %
inc(A) -> A + 1.
dec(A) -> A - 1.
plus(A, B) ->
   case (A == 0) of
      true -> b;
      false -> inc(dec(A) + B)
   end.
plus_1(A, B) ->
   case (A == 0) of
      true -> B;
      false -> dec(A) + inc(b)
   end.

% Exercise 1.10 %
a(_, 0) -> 0;
a(0, Y) -> 2 * Y;
a(_, 1) -> 2;
a(X, Y) -> a(X - 1, a(X, Y - 1)).

f_1(N) -> a(0, N).
g_1(N) -> a(1, N).
h_1(N) -> a(2, N).
k_1(N) -> 5 * N * N.

section_1_2_1() ->
   print (a(1, 10)),
   print (a(2, 4)),
   print (a(3, 3)).


% 1.2.2 Procedures and the Processes They Generate - Tree Recursion

% Recursive %
fib(0) -> 0;
fib(1) -> 1;
fib(N) -> fib(N - 1) + fib(N - 2).

% Iterative %
fib_iter(_, B, 0) -> B;
fib_iter(A, B, Count) -> fib_iter(A + B, A, Count - 1).

fib_1(N) -> fib_iter(1, 0, N).

% Counting change %
first_denomination(1) -> 1;
first_denomination(2) -> 5;
first_denomination(3) -> 10;
first_denomination(4) -> 25;
first_denomination(5) -> 50.

cc(Amount, Kinds_Of_Coins) ->
   case (Amount == 0) of
      true -> 1;
      false ->
         case (Amount < 0) of
            true -> 0;
            false ->
               case (Kinds_Of_Coins == 0) of
                  true -> 0;
                  false ->
                     cc(Amount, Kinds_Of_Coins - 1) +
                     cc(Amount - first_denomination(Kinds_Of_Coins), Kinds_Of_Coins)
               end
         end
   end.

count_change(Amount) ->
   cc(Amount, 5).

% Exercise 1.11 %
fi(N) ->
   if
      (N < 3) -> N;
      true    -> fi(N - 1) + 2 * fi(N - 2) + 3 * fi(N - 3)
   end.

fi_iter(A, B, C, Count) ->
   if
      (Count == 0) -> C;
      true         -> fi_iter(A + 2 * B + 3 * C, A, B, Count - 1)
   end.

fi_1(N) -> fi_iter(2, 1, 0, N).

% Exercise 1.12 %
pascals_triangle(N, K) ->
   if
      (N == 0) -> 1;
      (K == 0) -> 1;
      (N == k) -> 1;
      true -> pascals_triangle(N - 1, K - 1) + pascals_triangle(N - 1, K)
   end.

% Alternate implementation using pattern matching %
pascals_triangle_2(0, _) -> 1;
pascals_triangle_2(_, 0) -> 1;
pascals_triangle_2(N, N) -> 1;
pascals_triangle_2(N, K) -> pascals_triangle_2(N - 1, K - 1) + pascals_triangle_2(N - 1, N).

section_1_2_2() ->
   print (count_change(100)),
   print (fi(5)),
   print (fi_1(5)),
   print (pascals_triangle(5, 3)).

% 1.2.3 Procedures and the Processes They Generate - Orders of Growth

% Exercise 1.15 %
cube(X) -> X * X * X.
p_1(X) -> (3.0 * X) - (4.0 * cube(X)).
sine(Angle) ->
   case (abs_1(Angle) < 0.001) of
      true -> Angle;
      false -> p_1(sine(Angle / 3.0))
   end.

section_1_2_3() ->
   print (sine(60.0)).

% 1.2.4 Procedures and the Processes They Generate - Exponentiation

% Linear recursion %
expt(_, 0) -> 1;
expt(B, N) -> B * expt(B, (N - 1)).

% Linear iteration %
expt_iter(_, 0, Product) -> Product;
expt_iter(B, Counter, Product) ->
   expt_iter(B, Counter - 1, B * Product).
expt_1(B, N) ->
   expt_iter(B, N, 1).

% Logarithmic iteration %
even(N) -> ((N rem 2) == 0).

fast_expt(_, 0) -> 1;
fast_expt(B, N) ->
  case (even(N)) of
     true -> square(fast_expt(B, N div 2));
     false -> B * fast_expt(B, N - 1)
   end.

% Exercise 1.17 %
multiply(_, 0) -> 0;
multiply(A, B) ->
  A + (A * (B - 1)).

% Exercise 1.19 %
% exercise left to reader to solve for p' and q'
% fib_iter_2(_, B, _, _, 0) -> B;
% fib_iter_2(A, B, P, Q, Count) ->
%    case (even(Count)) of
%       true -> fib_iter_2(A, B, P', Q', Count div 2);
%       false -> fib_iter_2((B * Q) + (A * Q) + (A * P), (B * P) + (A * Q), P, Q, Count - 1)
%    end.
% fib_2(N) ->
%    fib_iter_2(1, 0, 0, 1, N).

section_1_2_4() ->
   print (even(3)).


% 1.2.5 Procedures and the Processes They Generate - Greatest Common Divisors
gcd(A, 0) -> A;
gcd(A, B) -> gcd(B, A rem B).

section_1_2_5() ->
   print (gcd(12, 8)).


% 1.2.6 Procedures and the Processes They Generate - Example: Testing for Primality

% prime %
divides(A, B) -> ((B rem A) == 0).

find_divisor(N, Test_Divisor) ->
   case (square(Test_Divisor) > N)of
      true -> N;
      false ->
         case (divides(Test_Divisor, N)) of
            true -> Test_Divisor;
            false -> find_divisor(N, Test_Divisor + 1)
         end
   end.

smallest_divisor(N) -> find_divisor(N, 2).

prime(N) -> (N == smallest_divisor(N)).

% fast_prime %
expmod(_, 0, _) -> 1;
expmod(Nbase, Nexp, M) ->
  case (even(Nexp)) of
     true -> square(expmod(Nbase, Nexp div 2, M)) rem M;
     false -> (Nbase * (expmod(Nbase, (Nexp - 1), M))) rem M
   end.

fermat_test(N) ->
   Try =
      fun (A) ->
         (expmod(A, N, N) == A)
      end,
   Try(1 + random:uniform(N - 1)).

fast_prime(_, 0) -> true;
fast_prime(N, Ntimes) ->
  case (fermat_test(N)) of
     true -> fast_prime(N, Ntimes - 1);
     false -> false
   end.

% Exercise 1.22 %
report_prime(Elapsed_Time) ->
   io:format(" *** ~w~n", [Elapsed_Time]).

get_time() ->
   case (now()) of
      {_, Secs, _} -> Secs
   end.

start_prime_test(N, Start_Time) ->
   case (prime(N)) of
      true -> report_prime(get_time() - Start_Time);
      false -> {}
   end.

timed_prime_test(N) ->
   start_prime_test(N, get_time()).

% Exercise 1.25 %
expmod_1(Nbase, Nexp, M) ->
   fast_expt(Nbase, Nexp) rem M.

% Exercise 1.26 %
expmod_2(_, 0, _) -> 1;
expmod_2(Nbase, Nexp, M) ->
   case (even(Nexp)) of
      true -> (expmod_2(Nbase, (Nexp div 2), M) *
               expmod_2(Nbase, (Nexp div 2), M)) rem M;
      false -> (Nbase * expmod_2(Nbase, Nexp - 1, M)) rem M
   end.

section_1_2_6() ->
   timed_prime_test(13).


% 1.3 Formulating Abstractions with Higher-Order Procedures
% same as above
% cube(X) -> X * X * X.


% 1.3.1 Formulating Abstractions with Higher-Order Procedures - Procedures as Arguments
sum_integers(A, B) ->
   case (A > B) of
      true -> 0;
      false -> A + sum_integers(A + 1, B)
   end.

sum_cubes (A, B) ->
   case (A > B) of
      true -> 0;
      false -> cube(A) + sum_cubes(A + 1, B)
   end.

pi_sum(A, B) ->
   case (A > B) of
      true -> 0.0;
      false -> (1.0 / (A * (A + 2.0))) + pi_sum(A + 4.0, B)
   end.

sum(Term, A, Next, B) ->
   case (A > B) of
      true -> 0;
      false -> Term(A) + sum(Term, Next(A), Next, B)
   end.

% Using sum %
% same as above
% inc(N) -> N + 1.

sum_cubes_1(A, B) ->
   sum(fun (X) -> cube(X) end, A, fun (X) -> inc(X) end, B).

identity(X) -> X.

sum_integers_1(A, B) ->
   sum(fun (X) -> identity(X) end, A, fun (X) -> inc(X) end, B).

sum_float(Term, A, Next, B) ->
   case (A > B) of
      true -> 0.0;
      false -> Term(A) + sum_float(Term, Next(A), Next, B)
   end.

pi_sum_1(A, B) ->
   Pi_Term = fun (X) -> 1 / (X * (X + 2)) end,
   Pi_Next = fun (X) -> X + 4.0 end,
   sum_float(Pi_Term, A, Pi_Next, B).

integral(F, A, B, Dx) ->
   Add_Dx = fun (X) -> X + Dx end,
   sum_float(F, A + (Dx / 2), Add_Dx, B) * Dx.

% Exercise 1.29 %
simpson(F, A, B, N) ->
   H = erlang:abs(B - A) / N,

   Sum_Iter = fun(Self, Term, Start, Next, Stop, Acc) ->
      if 
         (Start > Stop) -> Acc;
         true           -> Self(Self, Term, Next(Start), Next, Stop, Acc + Term(A + Start * H))
      end
   end,

   H * Sum_Iter(Sum_Iter, F, 1, fun inc/1, N, 0.0).

% Exercise 1.30 %
sum_iter(Term, A, Next, B, Acc) ->
   if
      (A > B) -> Acc;
      true    -> sum_iter(Term, Next(A), Next, B, Acc + Term(A))
   end.

% 'sum_cubes_2' reimplements 'sum_cubes_' but uses 'sum_iter' in place of 'sum'
sum_cubes_2(A, B) -> sum_iter(fun cube/1, A, fun inc/1, B, 0).

% Exercise 1.31 %
% a.
product(Term, A, Next, B) ->
   if
      (A > B) -> 1;
      true    -> Term(A) * product(Term, Next(A), Next, B)
   end.

factorial_2(N) -> product(fun identity/1, 1, fun inc/1, N).

% b.
product_iter(Term, A, Next, B, Acc) ->
   if
      (A > B) -> Acc;
      true    -> product_iter(Term, Next(A), Next, B, Acc * Term(A))
   end.

factorial_3(N) -> product_iter(fun identity/1, 1, fun inc/1, N, 1).

% Exercise 1.32 %
% a.
accumulate(Combiner, NullValue, Term, A, Next, B) ->
   if
      (A > B) -> NullValue;
      true    -> Combiner(Term(A), accumulate(Combiner, NullValue, Term, Next(A), Next, B))
   end.

% sum:     accumulate(fun plus/2, 0, fun identity/1, A, fun inc/1, B).
% product: accumulate(fun times/2, 1, fun identity/1, A, fun inc/1, B).

% b.
% NOTE: starting value of 'Acc' is 'NullValue'
accumulate_iter(Combiner, Term, A, Next, B, Acc) ->
   if
      (A > B) -> Acc;
      true    -> accumulate_iter(Combiner, Term, Next(A), Next, B, Combiner(Acc, Term(A)))
   end.

% sum:     accumulate_iter(fun plus/2, fun identity/1, A, fun inc/1, B, 0).
% product: accumulate_iter(fun times/2, fun identity/1, A, fun inc/1, B, 1).

% Exercise 1.33 %
filtered_accumulate(Combiner, NullValue, Term, A, Next, B, Pred) ->
   if
      (A > B) -> NullValue;
      true    ->
         case Pred(A) of
            true  -> Combiner(Term(A), filtered_accumulate(Combiner, NullValue, Term, Next(A), Next, B, Pred));
            false -> filtered_accumulate(Combiner, NullValue, Term, Next(A), Next, B, Pred)
         end
   end.

% a.
% filtered_accumulate(fun plus/2, 0, fun square/1, 1, fun inc/1, 5, fun prime/1);  % 39

% b. Not sure how to implement this without modifying 'filtered_accumulate' to have 'Pred'
%    accept two arguments

section_1_3_1() ->
   print (sum_cubes(1, 10)),
   print (sum_cubes_1(1, 10)),
   print (sum_integers(1, 10)),
   print (sum_integers_1(1, 10)),
   print (8.0 * pi_sum(1.0, 1000.0)),
   print (8.0 * pi_sum_1(1.0, 1000.0)),
   print (integral(fun (X) -> cube(X) end, 0.0, 1.0, 0.01)),
   print (integral(fun (X) -> cube(X) end, 0.0, 1.0, 0.001)),
   print(simpson(fun cube_1/1, 0.0, 1.0, 100)),
   print(sum_cubes_2(1, 10)),
   print(factorial_2(5)),
   print(factorial_3(5)),
   print(accumulate(fun plus/2, 0, fun identity/1, 1, fun inc/1, 5)),
   print(accumulate(fun times/2, 1, fun identity/1, 1, fun inc/1, 5)),
   print(accumulate_iter(fun plus/2, fun identity/1, 1, fun inc/1, 5, 0)),
   print(accumulate_iter(fun times/2, fun identity/1, 1, fun inc/1, 5, 1)),
   print(filtered_accumulate(fun plus/2, 0, fun square/1, 1, fun inc/1, 5, fun prime/1)).

% 1.3.2 Formulating Abstractions with Higher-Order Procedures - Constructing Procedures Using Lambda
pi_sum_2(A, B) ->
   sum_float(fun (X) -> 1.0 / (X * (X + 2.0)) end, A, fun (X) -> X + 4.0 end, B).

integral_1(F, A, B, Dx) ->
   sum_float(F, A + (Dx / 2), fun (X) -> X + Dx end, B) * Dx.

plus4(X) -> X + 4.

% Using let %
f_2(X, Y) ->
   F_Helper =
      fun (A, B) ->
         (X * square(A)) + (Y * B) + (A * B)
      end,
   F_Helper(1 + (X * Y), 1 - Y).

f_3(X, Y) ->
   (fun (A, B) -> (X * square(A)) + (Y * B) + (A * B) end)
      (1 + (X * Y), 1 - Y).

f_4(X, Y) ->
   A = 1 + (X * Y),
   B = 1 - Y,
   (X * square(A)) + (Y * B) + (A * B).

f_5(X, Y) ->
   A = 1 + (X * Y),
   B = 1 - Y,
   (X + square(A)) + (Y * B) + (A * B).

%% Exercise 1.34 %
f_6(G) -> G(2).

section_1_3_2() ->
   print (8.0 * pi_sum_2(1.0, 1000.0)),
   print (integral_1(fun (X) -> cube(X) end, 0.0, 1.0, 0.01)),
   Plus4 = fun (X) -> X + 4 end,
   print ((fun (X, Y, Z) -> X + Y + square(Z) end) (1, 2, 3)),

   % I need to figure out erlang's equivalent of let
   %X = 5,
   %let
   %   val X = 3
   %in
   %   X + (X * 10)
   %end + X;

   %val X = 2;
   %let
   %   val X = 3
   %   and Y = X + 2
   %in
   %   X * Y
   %end;

   % Exercise 1.34 %
   print (f_6(fun (X) -> square(X) end)),
   print (f_6(fun (Z) -> Z * (Z + 1) end)).


% 1.3.3 Formulating Abstractions with Higher-Order Procedures - Procedures as General Methods

% Half-interval method %
close_enough(X, Y) ->
   (abs(X - Y) < 0.001).

positive(X) -> (X >= 0.0).
negative(X) -> not(positive(X)).

search(F, Neg_Point, Pos_Point) ->
   MidPoint = average(Neg_Point, Pos_Point),
   case (close_enough(Neg_Point, Pos_Point)) of
      true -> MidPoint;
      false ->
         Test_Value = F(MidPoint),
         case (positive(Test_Value)) of
            true -> search(F, Neg_Point, MidPoint);
            false ->
               case (negative(Test_Value)) of
                  true -> search(F, MidPoint, Pos_Point);
                  false -> MidPoint
               end
         end
   end.

half_interval_method(F, A, B) ->
   A_Value = F(A),
   B_Value = F(B),
   case ((negative(A_Value)) and (positive(B_Value))) of
      true -> search(F, A, B);
      false ->
         case ((negative(B_Value)) and (positive(A_Value))) of
            true -> search(F, B, A);
            false -> {} %% raise Invalid("Values are not of opposite sign" ^ Real.toString(a) ^ " " ^ Real.toString(b))
         end
   end.

% Fixed points %
fixed_point(F, First_Guess) ->
   Tolerance = 0.00001,
   Close_Enough =
      fun (V1, V2) ->
         abs(V1 - V2) < Tolerance
      end,
   Try =
      fun (Self, Guess) ->
         Next = F(Guess),
         case (Close_Enough(Guess, Next)) of
            true -> Next;
            false -> Self(Self, Next)
         end
      end,
   Try(Try, First_Guess).

% note: this function does not converge %
sqrt_4(X) ->
   fixed_point(fun (Y) -> X / Y end, 1.0).

sqrt_5(X) ->
   fixed_point(fun (Y) -> average(Y, X / Y) end, 1.0).

% Exercise 1.35 %
golden_ratio() -> fixed_point(fun (X) -> 1.0 + 1.0 / X end, 1.0).

% Exercise 1.36 %
% Add the following line to function, 'fixed_point':
%  ... Next = F(Guess),
%  print(Next),
%  ... case ...

% Exercise 1.37 %
% exercise left to reader to define cont_frac
% cont_frac(fun (I) -> 1.0 end, fun (I) -> 1.0 end, K).

% Exercise 1.38 - unfinished %

% Exercise 1.39 - unfinished %

section_1_3_3() ->
   print (half_interval_method(fun (X) -> math:sin(X) end, 2.0, 4.0)),
   print (half_interval_method(fun (X) -> (X * X * X) - (2.0 * X) - 3.0 end, 1.0, 2.0)),

   print (fixed_point(fun (X) -> math:cos(X) end, 1.0)),
   print (fixed_point(fun (Y) -> math:sin(Y) + math:cos(Y) end, 1.0)),

   print (sqrt_5(25))

   print(golden_ratio()),
   print(fixed_point(fun(X) -> math:log(1000.0) / math:log(X) end, 1.5)),

   % Needs 'average_damp' from next section:
   %
   %    average_damp(F) -> fun (X) -> average(X, F(X)) end.
   %
   print(fixed_point(average_damp(fun(X) -> math:log(1000.0) / math:log(X) end), 1.5)).

% 1.3.4 Formulating Abstractions with Higher-Order Procedures - Procedures as Returned Values
average_damp(F) ->
   fun (X) -> average(X, F(X)) end.

sqrt_6(X) ->
   fixed_point(average_damp(fun (Y) -> X / Y end), 1.0).

cube_root(X) ->
   fixed_point(average_damp(fun (Y) -> X / square(Y) end), 1.0).

% Newton's method %
deriv(G) ->
   Dx = 0.00001,
   fun (X) -> (G(X + Dx) - G(X)) / Dx end.

% same as before
% cube(X) -> X * X * X.

newton_transform(G) ->
   fun (X) -> X - (G(X) / ((deriv(G))(X))) end.

newtons_method(G, Guess) ->
   fixed_point(newton_transform(G), Guess).

sqrt_7(X) ->
   newtons_method(fun (Y) -> square(Y) - X end, 1.0).

% Fixed point of transformed function %
fixed_point_of_transform (G, Transform, Guess) ->
   fixed_point(Transform(G), Guess).

sqrt_8(X) ->
   fixed_point_of_transform(
      fun (Y) -> X / Y end,
      fun (Y) -> average_damp(Y) end,
      1.0).

sqrt_9(X) ->
   fixed_point_of_transform(
      fun (Y) -> square(Y) - X end,
      fun (Y) -> newton_transform(Y) end,
      1.0).

% Exercise 1.40 %
cubic(A, B, C) -> fun (X) -> X * X * X + A * X * X + B * X + C end.

% Exercise 1.41 %
double(F) -> fun (X) -> F(F(X)) end.

% Exercise 1.42 %
compose(F, G) -> fun (X) -> F(G(X)) end.

% Exercise 1.43 %
repeated(F, N) ->
   Iterate = fun (Self, Arg, I) ->
      if 
         (I > N) -> Arg;
         true    -> Self(Self, F(Arg), I + 1)
      end
   end,
   fun (X) -> Iterate(Iterate, X, 1) end.

% Exercise 1.44 ('n-fold-smooth' not implemented) %
smooth(F, Dx) -> fun (X) -> average(X, (F(X - Dx) + F(X) + F(X + Dx)) / 3.0) end.

% Exercise 1.45 - unfinished %

% Exercise 1.46 ('sqrt' not implemented) %
iterative_improve(Good_enough, Improve) ->
   Iterate = fun (Self, Guess) ->
      Next = Improve(Guess),
      case Good_enough(Guess, Next) of
         true  -> Next;
         false -> Self(Self, Next)
      end
   end,
   fun (X) -> Iterate(Iterate, X) end.

fixed_point_(F, First_guess) ->
   Tolerance = 0.00001,
   Good_enough = fun (V1, V2) -> erlang:abs(V1 - V2) < Tolerance end,
   (iterative_improve(Good_enough, F))(First_guess).

section_1_3_4() ->
   print ((average_damp(fun (X) -> square(X) end)) (10.0)),
   print (sqrt_6(25)),
   print (cube_root(27)),
   print ((deriv(fun (X) -> cube(X) end)) (5.0)),
   print (sqrt_7(25)),
   print (sqrt_8(25)),
   print (sqrt_9(25)),
   print(newtons_method(cubic(5.0, 3.0, 2.5), 1.0)),
   print((double(fun inc/1))(5)),
   print((double(double(fun inc/1)))(5)),
   print((double(double(double(fun inc/1))))(5)),
   print((compose(fun square/1, fun inc/1))(6)),
   print((repeated(fun square/1, 2))(5)),
   print(fixed_point(smooth(fun (X) -> math:log(1000.0) / math:log(X) end, 0.05), 1.5)),
   print(fixed_point_(average_damp(fun (X) -> math:log(1000.0) / math:log(X) end), 1.5)).
