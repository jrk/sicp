(* SICP Chapter #01 Examples in Alice ML / Standard ML *)
(* Alice ML version *)
import structure Random from "x-alice:/lib/utility/Random"
(* end Alice ML version *)

(* 1.1.1 The Elements of Programming - Expressions *)
486;
137 + 349;
1000 - 334;
5 * 99;
10 div 5;
2.7 + 10.0;
21 + 35 + 12 + 7;
25 * 4 * 12;
3 * 5 + 10 - 6;
3 * (2 * 4 + 3 + 5) + 10 - 7 + 6;

(* 1.1.2 The Elements of Programming - Naming and the Environment *)
val size' = 2;
size';
5 * size';
val pi = 3.14159
val radius = 10.0;
pi * radius * radius;
val circumference = 2.0 * pi * radius;
circumference;

(* 1.1.3 The Elements of Programming - Evaluating Combinations *)
(2 + 4 * 6) * (3 + 5 + 7);

(* 1.1.4 The Elements of Programming - Compound Procedures *)
fun square x = x * x;
square 21;
square(2 + 5);
square(square 3);
fun sum_of_squares (x, y) = square x + square y;
sum_of_squares(3, 4);
fun f a = sum_of_squares(a + 1, a * 2);
f 5;

(* 1.1.5 The Elements of Programming - The Substitution Model for Procedure Application *)
f 5;
sum_of_squares(5 + 1, 5 * 2);
square 6 + square 10;
6 * 6 + 10 * 10;
36 + 100;
f 5;
sum_of_squares(5 + 1, 5 * 2);
square(5 + 1) + square(5 * 2);

((5 + 1) * (5 + 1)) + ((5 * 2) * (5 * 2));
(6 * 6) + (10 * 10);
36 + 100;
136;

(* 1.1.6 The Elements of Programming - Conditional Expressions and Predicates *)
fun abs' x =
   if x > 0 then x
   else if x = 0 then 0
   else ~x
fun abs' x =
   if x < 0
      then ~x
      else x
val x = 6;
x > 5 andalso x < 10;
fun ge (x, y) = x > y orelse x = y
fun ge (x, y) = not(x < y);

(* Exercise 1.1 *)
10;
5 + 3 + 4;
9 - 1;
6 div 2;
2 * 4 + 4 - 6;
val a = 3
val b = a + 1;
a + b + a * b;
a = b;
if b > a andalso b < a * b
   then b
   else a;
if a = 4
   then 6
   else
      if b = 4
         then 6 + 7 + a
         else 25;
2 + (if b > a then b else a);
(if a > b then a
 else if a < b then b
 else ~1) * (a + 1);

(* Exercise 1.2 *)
(5.0 + 4.0 + (2.0 - (3.0 - (6.0 + (4.0 / 5.0))))) /
   (3.0 * (6.0 - 2.0) * (2.0 - 7.0));

(* Exercise 1.3 *)
fun three_n(n1, n2, n3) =
   if n1 > n2
      then
         if n1 > n3
            then
               if n2 > n3
                  then n1*n1 + n2*n2
                  else n1*n1 + n3*n3
            else n1*n1 + n3*n3
      else
         if n2 > n3
            then
               if n1 > n3
                  then n2*n2 + n1*n1
                  else n2*n2 + n3*n3
            else n2*n2 + n3*n3

(* Exercise 1.4 *)
fun a_plus_abs_b (a, b) =
   if b > 0
      then a + b
      else a - b

(* Exercise 1.5 *)
fun p () = p()
fun test (x, y) =
   if x = 0
      then 0
      else y;
(* commented out as this is in infinite loop
   test(0, p()); *)

(* 1.1.7 The Elements of Programming - Example: Square Roots by Newton's Method *)
fun square_real x:real = x * x

fun good_enough (guess, x) =
   abs(square_real guess - x) < 0.001

fun average (x, y) =
   (x + y) / 2.0

fun improve (guess, x) =
   average(guess, x / guess)

fun sqrt_iter (guess, x) =
   if good_enough(guess, x)
      then guess
      else sqrt_iter(improve(guess, x), x)

fun sqrt x =
   sqrt_iter(1.0, x);

sqrt 9.0;
sqrt(100.0 + 37.0);
sqrt(sqrt 2.0 + sqrt 3.0);
square_real(sqrt 1000.0);

(* Exercise 1.6 *)
fun new_if (predicate, then_clause, else_clause) =
   if predicate
      then then_clause
      else else_clause;
new_if(2=3, 0, 5);
new_if(1=1, 0, 5);
fun sqrt_iter(guess, x) =
   new_if(
      good_enough(guess, x),
      guess,
      sqrt_iter(improve(guess, x), x))

(* from wadler paper *)
fun newif true  x y = x
  | newif false x y = y

(* Exercse 1.7 *)
fun good_enough_gp (guess, prev) =
   abs(guess - prev) / guess < 0.001

fun sqrt_iter_gp (guess, prev, x) =
   if good_enough_gp(guess, prev)
      then guess
      else sqrt_iter_gp(improve(guess, x), guess, x)

fun sqrt_gp x =
   sqrt_iter_gp(4.0, 1.0, x)

(* Exercise 1.8 *)
fun improve_cube (guess, x) =
   (2.0*guess + x/(guess * guess)) / 3.0

fun cube_iter (guess, prev, x) =
   if good_enough_gp(guess, prev)
      then guess
      else cube_iter(improve_cube(guess, x), guess, x)

fun cube_root_0 x =
   cube_iter(27.0, 1.0, x)

(* 1.1.8 The Elements of Programming - Procedures as Black-Box Abstractions *)
fun square_real x:real = x * x

fun double x:real = x + x

fun square_real x = Math.exp(double(Math.ln x))

fun good_enough (guess, x) =
   abs(square_real guess - x) < 0.001

fun improve (guess, x) =
   average(guess, x / guess)

fun sqrt_iter (guess, x) =
   if good_enough(guess, x)
      then guess
      else sqrt_iter(improve(guess, x), x)

fun sqrt x =
   sqrt_iter(1.0, x);

square_real 5.0;

(* Block-structured *)
fun sqrt x =
   let
      fun good_enough (guess, x) =
         abs(square_real guess - x) < 0.001

      fun improve (guess, x) =
         average(guess, x / guess)

      fun sqrt_iter (guess, x) =
         if good_enough(guess, x)
            then guess
            else sqrt_iter(improve(guess, x), x)
   in
      sqrt_iter(1.0, x)
   end

(* Taking advantage of lexical scoping *)
fun sqrt x =
   let
      fun good_enough guess =
         abs(square_real guess - x) < 0.001

      fun improve guess =
         average(guess, x / guess)

      fun sqrt_iter guess =
         if good_enough guess
            then guess
            else sqrt_iter(improve guess)
   in
      sqrt_iter 1.0
   end

(* 1.2.1 Procedures and the Processes They Generate - Linear Recursion and Iteration *)

(* Recursive *)
fun factorial n =
   if n = 1
      then 1
      else n * factorial(n - 1);

factorial 6;

(* Iterative *)
fun fact_iter (product, counter, max_count) =
   if counter > max_count
      then product
      else fact_iter(counter * product, counter + 1, max_count)

fun factorial n =
   fact_iter(1, 1, n)

(* Iterative, block-structured (from footnote) *)
fun factorial n =
   let
      fun iter (product, counter) =
         if counter > n
            then product
            else iter(counter * product, counter + 1)
   in
      iter(1, 1)
   end

(* Exercise 1.9 *)
fun inc a = a + 1
fun dec a = a - 1
fun plus (a, b) =
   if a = 0
      then b
      else inc(plus(dec a, b))
fun plus (a, b) =
   if a = 0
      then b
      else plus(dec a, inc b)

(* Exercise 1.10 *)
fun a (x, 0) = 0
  | a (0, y) = 2 * y
  | a (x, 1) = 2
  | a (x, y) = a(x - 1, a(x, y - 1));
a(1, 10);
a(2, 4);
a(3, 3);
fun f n = a(0, n)
fun g n = a(1, n)
fun h n = a(2, n)
fun k n = 5 * n * n

(* 1.2.2 Procedures and the Processes They Generate - Tree Recursion *)

(* Recursive *)
fun fib 0 = 0
  | fib 1 = 1
  | fib n = fib(n - 1) + fib(n - 2)

(* Iterative *)
fun fib_iter (a, b, 0) = b
  | fib_iter (a, b, count) = fib_iter(a + b, a, count - 1)
fun fib n =
   fib_iter(1, 0, n)

(* Counting change *)
fun first_denomination 1 = 1
  | first_denomination 2 = 5
  | first_denomination 3 = 10
  | first_denomination 4 = 25
  | first_denomination 5 = 50
  | first_denomination x = raise Domain

fun cc (amount, kinds_of_coins) =
   if amount = 0 then 1
   else if amount < 0 then 0
   else if kinds_of_coins = 0 then 0
   else cc(amount, kinds_of_coins - 1) +
        cc(amount - first_denomination kinds_of_coins, kinds_of_coins)

fun count_change amount =
   cc(amount, 5);

count_change 100;

(* Exercise 1.11 *)
fun f n =
   if n < 3
      then n
      else f(n-1) + 2*f(n-2) + 3*f(n-3)

fun f_iter (a, b, c, 0)     = c
  | f_iter (a, b, c, count) = f_iter(a + 2*b + 3*c, a, b, count-1)

fun f n = f_iter(2, 1, 0, n)

(* Exercise 1.12 *)
fun pascals_triangle (0, k) = 1
  | pascals_triangle (n, 0) = 1
  | pascals_triangle (n, k) =
      if n = k
         then 1
         else pascals_triangle(n-1, k-1) + pascals_triangle(n-1, k)

(* 1.2.3 Procedures and the Processes They Generate - Orders of Growth *)

(* Exercise 1.15 *)
fun cube x:real = x * x * x
fun p x = 3.0 * x - 4.0 * cube x
fun sine angle =
   if not(abs angle > 0.1)
      then angle
      else p(sine angle/3.0)

(* 1.2.4 Procedures and the Processes They Generate - Exponentiation *)

(* Linear recursion *)
fun expt (b, 0) = 1
  | expt (b, n) = b * expt(b, n - 1)

(* Linear iteration *)
fun expt_iter (b, 0, product) = product
  | expt_iter (b, counter, product) =
      expt_iter(b, counter - 1, b * product)
fun expt(b, n) =
   expt_iter(b, n, 1)

(* Logarithmic iteration *)
fun even n = (n mod 2 = 0)

fun fast_expt(b, 0) = 1
  | fast_expt(b, n) =
      if even n
         then square(fast_expt(b, n div 2))
         else b * fast_expt(b, n - 1)

(* Exercise 1.17 *)
fun multiply (a, 0) = 0
  | multiply (a, b) = a + a*(b - 1)

(* Exercise 1.19 *)
fun fib_iter(a, b, p, q, 0) = b
  | fib_iter(a, b, p, q, count) =
      if even count
         then fib_iter(a, b, p*p + q*q, 2*p*q + q*q, count div 2)
         else fib_iter(b*q + a*q + a*p, b*p + a*q, p, q, count - 1)
fun fib n =
   fib_iter(1, 0, 0, 1, n)

(* 1.2.5 Procedures and the Processes They Generate - Greatest Common Divisors *)
fun gcd (a, 0) = a
  | gcd (a, b) = gcd(b, a mod b);

gcd(40, 6);

(* Exercise 1.20 *)
gcd(206, 40);

(* 1.2.6 Procedures and the Processes They Generate - Example: Testing for Primality *)

(* prime *)
fun divides (a, b) = (b mod a = 0)

fun find_divisor(n, test_divisor) =
   if square(test_divisor) > n then n
   else if divides(test_divisor, n) then test_divisor
   else find_divisor(n, test_divisor + 1)

fun smallest_divisor n = find_divisor(n, 2)

fun prime n = (n = smallest_divisor n)

(* fast_prime *)
fun expmod (nbase, 0, m) = 1
  | expmod (nbase, nexp, m) =
      if even nexp
         then square(expmod(nbase, nexp div 2, m)) mod m
         else (nbase * expmod(nbase, nexp - 1, m)) mod m

(* Alice ML version *)
fun fermat_test n =
   let
      fun try_it a = (expmod(a, n, n) = a)
   in
      try_it(1 + Random.int(n - 1))
   end
(* end Alice ML version *)

(* SML-NJ version *)
(*
   val randGen = Random.rand(0, 10)
   fun fermat_test n =
      let
         fun try_it a = (expmod(a, n, n) = a)
      in
         try_it(1 + (Random.randRange(0, n-1) randGen))
      end
 *)
(* end SML-NJ version *)

fun fast_prime (n, 0) = true
  | fast_prime (n, ntimes) =
      if fermat_test n
         then fast_prime(n, ntimes - 1)
         else false;

(* Exercise 1.21 *)
smallest_divisor 199;
smallest_divisor 1999;
smallest_divisor 19999;

(* Exercise 1.22 *)
fun report_prime elapsed_time =
   print (" *** " ^ IntInf.toString elapsed_time)
fun start_prime_test (n, start_time) =
   if prime n
      then report_prime(Time.toMilliseconds(Time.now()) - start_time)
      else ()
fun timed_prime_test n =
   let
      val _ = print("\n" ^ Int.toString n)
   in
      start_prime_test(n, Time.toMilliseconds(Time.now()))
   end

(* Exercise 1.25 *)
fun expmod (nbase, nexp, m) =
   fast_expt(nbase, nexp) mod m

(* Exercise 1.26 *)
fun expmod (nbase, 0, m) = 1
  | expmod (nbase, nexp, m) =
      if even nexp
         then (expmod(nbase, nexp div 2, m) * expmod(nbase, nexp div 2, m)) mod m
         else (nbase * expmod(nbase, nexp - 1, m)) mod m

(* Exercise 1.27 *)
fun carmichael n =
   fast_prime(n, 100) andalso not(prime n);

carmichael 561;
carmichael 1105;
carmichael 1729;
carmichael 2465;
carmichael 2821;
carmichael 6601;

(* 1.3 Formulating Abstractions with Higher-Order Procedures *)
fun cube x = x * x * x

(* 1.3.1 Formulating Abstractions with Higher-Order Procedures - Procedures as Arguments *)
fun sum_integers (a, b) =
   if a > b
      then 0
      else a + sum_integers(a + 1, b)

fun sum_cubes (a, b) =
   if a > b
      then 0
      else cube a + sum_cubes(a + 1, b)

fun pi_sum (a, b) =
   if a > b
      then 0.0
      else 1.0/(a*(a + 2.0)) + pi_sum(a + 4.0, b)

fun sum (term, a, next, b) =
   if a > b
      then 0
      else term a + sum(term, next a, next, b)

(* Using sum *)
fun inc n = n + 1

fun sum_cubes (a, b) =
   sum(cube, a, inc, b);

sum_cubes(1, 10);

fun identity x = x

fun sum_integers (a, b) =
   sum(identity, a, inc, b);

sum_integers(1, 10);

fun sum_real (term, a:real, next, b) =
   if a > b
      then 0.0
      else term a + sum_real(term, next a, next, b)

fun pi_sum (a, b) =
   let
      fun pi_term x = 1.0 / (x * (x + 2.0))
      fun pi_next x = x + 4.0
   in
      sum_real(pi_term, a, pi_next, b)
   end;

8.0 * pi_sum(1.0, 1000.0);

fun integral (f, a, b, dx) =
   let
      fun add_dx x = x + dx
   in
      sum_real(f, a + dx/2.0, add_dx, b) * dx
   end

fun cube_real x:real = x * x * x;

integral(cube_real, 0.0, 1.0, 0.01);
integral(cube_real, 0.0, 1.0, 0.001);

(* Exercise 1.29 *)
fun simpson (f, a, b, n) =
   let
      val h = abs(b-a) / (Real.fromInt n)
      fun sum_iter (term, start, next, stop, acc : real) =
         if start > stop
            then acc
            else sum_iter(term, next start, next, stop, acc + term(a + (Real.fromInt start)*h))
   in
      h * sum_iter(f, 1, inc, n, 0.0)
   end;
simpson(cube_real, 0.0, 1.0, 100);

(* Exercise 1.30 *)
fun sum_iter (term, a, next, b, acc) =
   if a > b
      then acc
      else sum_iter(term, next a, next, b, acc + (term a))
fun sum_cubes (a, b) =
   sum_iter(cube, a, inc, b, 0);
sum_cubes(1, 10);

(* Exercise 1.31 *)
fun product (term, a, next, b) =
   if a > b
      then 1
      else (term a) * product(term, next a, next, b)
fun factorial n =
   product(identity, 1, inc, n)

fun product_iter (term, a, next, b, acc) =
   if a > b
      then acc
      else product_iter(term, next a, next, b, acc * (term a))

(* Exercise 1.32 *)
fun accumulate (combiner, null_value, term, a, next, b) =
   if a > b
      then null_value
      else combiner(term a, accumulate(combiner, null_value, term, next a, next, b))
fun sum (a, b) = accumulate(op+, 0, identity, a, inc, b)
fun product (a, b) = accumulate(op*, 1, identity, a, inc, b)

fun accumulate_iter (combiner, term, a, next, b, acc) =
   if a > b
      then acc
      else accumulate_iter(combiner, term, next a, next, b, combiner(acc, term a))
fun sum (a, b) = accumulate_iter(op+, identity, a, inc, b, 0)
fun product (a, b) = accumulate_iter(op*, identity, a, inc, b, 1)

(* Exercise 1.33 *)
fun filtered_accumulate(combiner, null_value, term, a, next, b, pred) =
   if a > b
      then null_value
      else
         if pred a
            then combiner(term a, filtered_accumulate(combiner, null_value, term, next a, next, b, pred))
            else filtered_accumulate(combiner, null_value, term, next a, next, b, pred);
filtered_accumulate(op+, 0, square, 1, inc, 5, prime);

(* 1.3.2 Formulating Abstractions with Higher-Order Procedures - Constructing Procedures Using Lambda *)
fun pi_sum (a, b) =
  sum_real(fn x => 1.0 / (x * (x + 2.0)), a, fn x => x + 4.0, b)

fun integral (f, a, b, dx) =
   sum_real(f, a + dx/2.0, fn x => x + dx, b) * dx

fun plus4 x = x + 4

val plus4 = fn x => x + 4;

(fn (x, y, z) => x + y + square z) (1, 2, 3);

(* Using let *)
fun f (x, y) =
   let
      fun f_helper (a, b) =
         x * square a + y*b + a*b
   in
      f_helper(1 + x*y, 1 - y)
   end

fun f (x, y) =
   (fn (a, b) => x * square a + y*b + a*b)
      (1 + x*y, 1 - y)

fun f (x, y) =
   let
      val a = 1 + x*y
      val b = 1 - y
   in
      x * square a + y*b + a*b
   end

val x = 5;
let
   val x = 3
in
   x + x*10
end + x;

val x = 2;
let
   val x = 3
   and y = x + 2
in
   x * y
end;

fun f (x, y) =
   let
      val a = 1 + x*y
      val b = 1 - y
   in
      x + square a + y*b + a*b
   end

(* Exercise 1.34 *)
fun f g = g 2;
f square;
f(fn z => z * (z + 1));

(* 1.3.3 Formulating Abstractions with Higher-Order Procedures - Procedures as General Methods *)

(* Half-interval method *)
fun close_enough (x, y) =
   (abs(x - y) < 0.001)

fun positive x = (x >= 0.0)
fun negative x = not(positive x)

fun search(f, neg_point, pos_point) =
   let
      val midpoint = average(neg_point, pos_point)
   in
      if close_enough(neg_point, pos_point)
         then midpoint
         else
            let
               val test_value = f midpoint
            in
               if positive test_value      then search(f, neg_point, midpoint)
               else if negative test_value then search(f, midpoint, pos_point)
               else midpoint
            end
   end

exception Invalid of string
fun half_interval_method (f, a, b) =
   let
      val a_value = f a
      val b_value = f b
   in
      if negative a_value andalso positive b_value      then search(f, a, b)
      else if negative b_value andalso positive a_value then search(f, b, a)
      else raise Invalid("Values are not of opposite sign" ^ Real.toString a ^ " " ^ Real.toString b)
   end;

half_interval_method(Math.sin, 2.0, 4.0);

half_interval_method(fn x => x*x*x - 2.0*x - 3.0, 1.0, 2.0);

(* Fixed points *)
val tolerance = 0.00001

fun fixed_point (f, first_guess) =
   let
      fun close_enough (v1, v2) =
         abs(v1 - v2) < tolerance
      fun try guess =
         let
            val next = f guess
         in
            if close_enough(guess, next)
               then next
               else try(next)
         end
   in
      try first_guess
   end;

fixed_point(Math.cos, 1.0);

fixed_point(fn y => Math.sin y + Math.cos y, 1.0);

(* note: this function does not converge *)
fun sqrt x =
   fixed_point(fn y => x / y, 1.0)

fun sqrt x =
   fixed_point(fn y => average(y, x / y), 1.0)

(* Exercise 1.35 *)
fun goldenRatio () =
  fixed_point(fn x => 1.0 + 1.0/x, 1.0);

(* Exercise 1.36 *)
(* 35 guesses before convergence *)
fixed_point(fn x => (Math.ln 1000.0) / (Math.ln x), 1.5);
(* 11 guesses before convergence (AverageDamp defined below) *)
(* fixed_point(average_damp(fn x => (Math.ln 1000.0) / (Math.ln x)), 1.5); *)

(* Exercise 1.37 *)
(* exercise left to reader to define cont_frac
   cont_frac(fn i => 1.0, fn i => 1.0, k); *)

(* 1.3.4 Formulating Abstractions with Higher-Order Procedures - Procedures as Returned Values *)
fun average_damp f x =
   average(x, f x);

average_damp square_real 10.0;

fun sqrt x =
   fixed_point(average_damp (fn y => x / y), 1.0)

fun cube_root x =
   fixed_point(average_damp(fn y => x / square_real y), 1.0)

(* Newton's method *)
val dx = 0.00001
fun deriv g x =
   (g(x + dx) - g x) / dx

fun cube x:real = x * x * x;

deriv cube 5.0;

fun newton_transform g x =
   x - g x / deriv g x

fun newtons_method (g, guess) =
   fixed_point(newton_transform g, guess)

fun sqrt x =
   newtons_method(fn y => square_real y - x, 1.0)

(* Fixed point of transformed function *)
fun fixed_point_of_transform (g, transform, guess) =
   fixed_point(transform g, guess)

fun sqrt x =
   fixed_point_of_transform(fn y => x / y, average_damp, 1.0)

fun sqrt x =
   fixed_point_of_transform(fn y => square_real y - x,  newton_transform, 1.0)

(* Exercise 1.40 *)
fun cubic (a, b, c) =
   fn x => (cube x) + a*x*x + b*x + c;
newtons_method(cubic(5.0, 3.0, 2.5), 1.0);

(* Exercise 1.41 *)
fun double_ f =
   fn x => f(f x);
(double_ inc)(5);
((double_ double_) inc)(5);
((double_ (double_ double_)) inc)(5);

(* Exercise 1.42 *)
fun compose f g =
   fn x => f(g x);
(compose square inc) 6;

(* Exercise 1.43 *)
fun repeated (f, n) =
   let
      fun iterate (arg, i) =
         if i > n
            then arg
            else iterate(f arg, i+1)
in
   fn x => iterate(x, 1)
end;
(repeated(square, 2)) 5;

(* Exercise 1.44 *)
fun smooth (f, dx) =
   fn x => average(x, (f(x-dx) + f(x) + f(x+dx)) / 3.0);
fixed_point(smooth(fn x => (Math.ln 1000.0) / (Math.ln x), 0.05), 1.5);

(* Exercise 1.46 *)
fun iterative_improve (good_enough, improve) =
   let
      fun iterate guess =
         let
            val next = improve guess
         in
            if good_enough(guess, next)
               then next
               else iterate next
         end
   in
      fn x => iterate x
   end
fun fixed_point (f, first_guess) =
   let
      val tolerance = 0.00001
      fun good_enough (v1, v2) =
         abs(v1-v2) < tolerance
   in
      (iterative_improve(good_enough, f)) first_guess
   end;
fixed_point(average_damp(fn x => (Math.ln 1000.0) / (Math.ln x)), 1.5);
