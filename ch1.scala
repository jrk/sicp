object SICP01 {
   def main(args: Array[String]): unit = {

// 1.1.1 The Elements of Programming - Expressions
486;
137 + 349;
1000 - 334;
5 * 99;
10 / 5;
2.7 + 10.0;
21 + 35 + 12 + 7;
25 * 4 * 12;
(3 * 5) + (10 - 6);
(3 * ((2 * 4) + (3 + 5))) + ((10 - 7) + 6);

// 1.1.2 The Elements of Programming - Naming and the Environment
val size = 2;
size;
5 * size;
val pi = 3.14159;
val radius = 10.0;
pi * radius * radius;
val circumference = 2.0 * pi * radius;
circumference;

// 1.1.3 The Elements of Programming - Evaluating Combinations
(2 + (4 * 6)) * (3 + 5 + 7);

// 1.1.4 The Elements of Programming - Compound Procedures
def square(x: long) = x * x;
square(21);
square(2 + 5);
square(square(3));
def sum_of_squares(x: long, y: long) = square(x) + square(y);
sum_of_squares(3, 4);
def f(a: long) = sum_of_squares(a + 1, a * 2);
f(5);

// 1.1.5 The Elements of Programming - The Substitution Model for Procedure Application
f(5);
sum_of_squares(5 + 1, 5 * 2);
square(6) + square(10);
(6 * 6) + (10 * 10);
36 + 100;
f(5);
sum_of_squares(5 + 1, 5 * 2);
(square(5 + 1)) + (square(5 * 2));

((5 + 1) * (5 + 1)) + ((5 * 2) * (5 * 2));
(6 * 6) + (10 * 10);
36 + 100;
136;

// 1.1.6 The Elements of Programming - Conditional Expressions and Predicates
def abs(x: long) =
   if (x > 0)
      x
      else if (x == 0) 0
      else -x;
def abs_1(x: long) =
   if (x < 0)
      -x
      else x
val x = 6;
(x > 5) && (x < 10);
def ge(x: long, y: long) =
   (x > y) || (x == y);
def ge_1(x: long, y: long) =
   !(x < y);

/* Exercise 1.1 */
10;
5 + 3 + 4;
9 - 1;
6 / 2;
(2 * 4) + (4 - 6);
val a = 3;
val b = a + 1;
a + b + (a * b);
(a == b);
if ((b > a) && (b < (a *b)))
   b
   else a;
if (a == 4)
   6
   else if (b == 4) 6 + 7 + a
   else 25;
2 + (if (b > a) b else a);
(if (a > b)
   a
   else if (a < b) b
   else -1) * (a + 1);

/* Exercise 1.2 */
(5.0 + 4.0 + (2.0 - (3.0 - (6.0 + (4.0 / 5.0))))) /
   (3.0 * (6.0 - 2.0) * (2.0 - 7.0));

/* Exercise 1.4 */
def a_plus_abs_b(a: long, b: long) =
   if (b > 0)
      a + b
      else a - b;

/* Exercise 1.5 */
def p(): Unit = p();
def test(x: long, y: long) =
   if (x == 0)
      0
      else y;
/* commented out as this is in infinite loop
   test(0, p());
*/

// 1.1.7 The Elements of Programming - Example: Square Roots by Newton's Method
def square_double(x: double) = x * x;
def abs_double (x: double) = if (x < 0) -x else x

def good_enough(guess: double, x: double) =
   abs_double(square_double(guess) - x) < 0.001;

def average(x: double, y: double) =
   (x + y) / 2.0;

def improve(guess: double, x: double) =
   average(guess, x / guess);

def sqrt_iter(guess: double, x: double): double =
   if (good_enough(guess, x))
      guess
      else sqrt_iter(improve(guess, x), x);

def sqrt(x: double) =
   sqrt_iter(1.0, x);

sqrt(9.0);
sqrt(100.0 + 37.0);
sqrt((sqrt(2.0))+(sqrt(3.0)));
square_double(sqrt(1000.0));

/* Exercise 1.6 */
def new_if(predicate: boolean, then_clause: double, else_clause: double) =
   if (predicate)
      then_clause
      else else_clause;
new_if((2==3), 0, 5);
new_if((1==1), 0, 5);
def sqrt_iter_(guess: double, x: double): double =
   new_if(
      good_enough(guess, x),
      guess,
      sqrt_iter_(improve(guess, x), x));

// 1.1.8 The Elements of Programming - Procedures as Black-Box Abstractions
def square_double_1(x: double) = x * x;

def doublex(x: double) = x + x;

def square_double_2(x: double) = Math.exp(doublex(Math.log(x)));

def good_enough_1(guess: double, x: double) =
   abs_double((square_double_2(guess)) - x) < 0.001;

def improve_1(guess: double, x: double) =
   average(guess, x / guess);

def sqrt_iter_1(guess: double, x: double): double =
   if (good_enough(guess, x))
      guess
      else sqrt_iter_1(improve_1(guess, x), x);

def sqrt_1(x: double) =
   sqrt_iter_1(1.0, x);

square_double_1(5.0);

// Block-structured
def sqrt_2(x: double) =
{
   def good_enough(guess: double, x: double) =
      abs_double(square_double(guess) - x) < 0.001;

   def improve(guess: double, x: double) =
      average(guess, x / guess);

   def sqrt_iter(guess: double, x: double): double =
      if (good_enough(guess, x))
         guess
         else sqrt_iter(improve(guess, x), x)

   sqrt_iter(1.0, x);
};

// Taking advantage of lexical scoping
def sqrt_3(x: double) = {
   def good_enough(guess: double) =
      abs_double(square_double(guess) - x) < 0.001;

   def improve(guess: double) =
      average(guess, x / guess);

   def sqrt_iter(guess: double): double =
      if (good_enough(guess))
         guess
         else sqrt_iter(improve(guess));

   sqrt_iter(1.0);
};

// 1.2.1 Procedures and the Processes They Generate - Linear Recursion and Iteration

// Recursive
def factorial(n: long): long =
   if (n == 1)
      1
      else n * factorial(n - 1);

// Iterative
def fact_iter(product: long, counter: long, max_count: long): long =
   if (counter > max_count)
      product
      else fact_iter(counter * product, counter + 1, max_count);

def factorial_1(n: long) =
   fact_iter(1, 1, n);

// Iterative, block-structured (from footnote)
def factorial_2(n: long) = {
   def iter(product: long, counter: long): long =
      if (counter > n)
         product
         else iter(counter * product, counter + 1);

   iter(1, 1);
};

/* Exercise 1.9 */
def inc(a: long) = a + 1;
def dec(a: long) = a - 1;
def plus(a: long, b: long) =
   if (a == 0)
      b
      else inc(dec(a) + b);
def plus_1(a: long, b: long) =
   if (a == 0)
      b
      else dec(a) + inc(b);

/* Exercise 1.10 */
def a_(x: long, y: long): long =
   Pair(x, y) match {
      case Pair(x, 0) => 0;
      case Pair(0, y) => 2 * y;
      case Pair(x, 1) => 2;
      case Pair(x, y) => a_(x - 1, a_(x, y - 1));
   };
a_(1, 10);
a_(2, 4);
a_(3, 3);
def f_(n: long) = a_(0, n);
def g_(n: long) = a_(1, n);
def h_(n: long) = a_(2, n);
def k_(n: long) = 5 * n * n;

// 1.2.2 Procedures and the Processes They Generate - Tree Recursion

// Recursive
def fib(n: long): long =
   n match {
      case 0 => 0;
      case 1 => 1;
      case n => fib(n - 1) + fib(n - 2);
   };

// Iterative
def fib_iter(a: long, b: long, count: long): long =
   count match {
      case 0 => b;
      case count => fib_iter(a + b, a, count - 1);
   }
def fib_1(n: long) =
   fib_iter(1, 0, n);

// Counting change
def first_denomination(x: long) =
   x match {
      case 1 => 1;
      case 2 => 5;
      case 3 => 10;
      case 4 => 25;
      case 5 => 50;
      case x => throw new Exception("Domain");
   };

def cc(amount: long, kinds_of_coins: long): long =
   if (amount == 0)
      1
      else if (amount < 0) 0
      else if (kinds_of_coins == 0) 0
      else cc(amount, kinds_of_coins - 1) +
           cc(amount - first_denomination(kinds_of_coins), kinds_of_coins);

def count_change(amount: long) =
   cc(amount, 5);

count_change(100);

/* Exercise 1.11 */ 
def fi(n: long): long =
   if (n < 3)
      n;
      else fi(n - 1) + 2 * fi(n - 2) + 3 * fi(n - 3);

def fi_iter(a: long, b: long, c: long, count: long): long =
   if (count == 0)
      c;
      else fi_iter(a + 2 * b + 3 * c, a, b, count - 1);

def fi_1(n: long): long = fi_iter(2, 1, 0, n);

/* Exercise 1.12 */
def pascals_triangle(n: long, k: long): long =
   if (n == 0 || k == 0 || n == k)
      1;
      else pascals_triangle(n - 1, k - 1) + pascals_triangle(n - 1, k);

// 1.2.3 Procedures and the Processes They Generate - Orders of Growth

/* Exercise 1.15 */
def cube_double(x:double) = x * x * x;
def p_(x: double) = (3.0 * x) - (4.0 * cube_double(x));
def sine(angle: double): double =
   if (!(abs_double(angle) > 0.1))
      angle
      else p_(sine(angle / 3.0));

// 1.2.4 Procedures and the Processes They Generate - Exponentiation

// Linear recursion
def expt(b: long, n: long): long =
   n match {
      case 0 => 1;
      case n => b * expt(b, (n - 1));
   };

// Linear iteration
def expt_iter(b: long, counter: long, product: long): long =
   counter match {
      case 0 => product;
      case counter => expt_iter(b, counter - 1, b * product);
   };
def expt_1(b: long, n: long) =
   expt_iter(b, n, 1);

// Logarithmic iteration
def even(n: long) = ((n % 2) == 0);

def fast_expt(b: long, n: long): long =
   n match {
      case 0 => 1;
      case n =>
         if (even(n))
            square(fast_expt(b, n / 2))
            else b * fast_expt(b, n - 1);
   };

/* Exercise 1.17 */
def multiply(a: long, b: long) =
   b match {
      case 0 => 0;
      case b => a + (a * (b - 1));
   };

/* Exercise 1.19 */
/* exercise left to reader to solve for p' and q'
   def fib_iter_(a: long, b: long, p: long, q: long, count: long): long =
      count match {
         case 0 => b;
         case count =>
            if (even(count))
               fib_iter_(a, b, p', q', count / 2);
               else fib_iter_((b * q) + (a * q) + (a * p), (b * p) + (a * q), p, q, count - 1);
      };
   def fib_2(n: long) =
      fib_iter_(1, 0, 0, 1, n);
*/

// 1.2.5 Procedures and the Processes They Generate - Greatest Common Divisors
def gcd(a: long, b: long): long =
   b match {
      case 0 => a;
      case b => gcd(b, a % b);
   };

// 1.2.6 Procedures and the Processes They Generate - Example: Testing for Primality

// prime
def divides(a: long, b: long) = ((b % a) == 0);

def find_divisor(n: long, test_divisor: long): long =
   if (square(test_divisor) > n)
      n
      else if (divides(test_divisor, n)) test_divisor
      else find_divisor(n, test_divisor + 1);

def smallest_divisor(n: long) = find_divisor(n, 2);

def prime(n: long) = (n == smallest_divisor(n));

// fast_prime
def expmod(nbase: long, nexp: long, m: long): long =
   nexp match {
      case 0 => 1;
      case nexp =>
         if (even(nexp))
            square(expmod(nbase, nexp / 2, m)) % m
            else (nbase * (expmod(nbase, (nexp - 1), m))) % m;
   };

def fermat_test(n: long) = {
   def try_it(a: long) = (expmod(a, n, n) == a)
   try_it(1 + Math.round(Math.random() * (n - 1)));
};

def fast_prime(n: long, ntimes: long): boolean =
   ntimes match {
      case 0 => true;
      case ntimes =>
         if (fermat_test(n))
            fast_prime(n, ntimes - 1)
            else false;
   };

/* Exercise 1.22 */
def report_prime(elapsed_time: long) =
   System.out.println(" *** " + elapsed_time);
def start_prime_test(n: long, start_time: long) =
   if (prime(n))
      report_prime(java.util.Calendar.getInstance().get(java.util.Calendar.MILLISECOND) - start_time)
      else ();
def timed_prime_test(n: long) = {
   System.out.println(n);
   start_prime_test(n, java.util.Calendar.getInstance().get(java.util.Calendar.MILLISECOND));
};

/* Exercise 1.25 */
def expmod_(nbase: long, nexp: long, m: long) =
   fast_expt(nbase, nexp) % m;

/* Exercise 1.26 */
def expmod_2(nbase: long, nexp: long, m: long): long =
   nexp match {
      case 0 => 1;
      case nexp =>
         if (even(nexp))
            (expmod_2(nbase, (nexp / 2), m) * expmod_2(nbase, (nexp / 2), m)) % m
            else (nbase * expmod_2(nbase, nexp - 1, m)) % m;
   };

// 1.3 Formulating Abstractions with Higher-Order Procedures
def cube(x: long) = x * x * x;

// 1.3.1 Formulating Abstractions with Higher-Order Procedures - Procedures as Arguments
def sum_integers(a: long, b: long): long =
   if (a > b)
      0
      else a + sum_integers(a + 1, b);

def sum_cubes(a: long, b: long): long =
   if (a > b)
      0
      else cube(a) + sum_cubes(a + 1, b);

def pi_sum(a: double, b: double): double =
   if (a > b)
      0.0
      else (1.0 / (a * (a + 2.0))) + pi_sum(a + 4.0, b);

def sum(term: long=>long, a: long, next: long=>long, b: long): long =
   if (a > b)
      0
      else term(a) + sum(term, next(a), next, b);

// Using sum
def inc_(n:long) = n + 1;

def sum_cubes_(a: long, b: long) =
   sum(cube, a, inc_, b);

sum_cubes_(1, 10);

def identity(x: long) = x

def sum_integers_(a: long, b: long) =
   sum(identity, a, inc_, b);

sum_integers_(1, 10);

def sum_double(term: double=>double, a: double, next: double=>double, b: double): double =
   if (a > b)
      0.0
      else term(a) + sum_double(term, next(a), next, b: double);

def pi_sum_(a: double, b: double) = {
   def pi_term(x: double) = 1.0 / (x * (x + 2.0))
   def pi_next(x: double) = x + 4.0
   sum_double(pi_term, a, pi_next, b)
};

8.0 * pi_sum_(1.0, 1000.0);

def integral(f: double=>double, a: double, b: double, dx: double) = {
   def add_dx(x: double) = x + dx
   sum_double(f, a + (dx / 2.0), add_dx, b) * dx
};

def cube_double_(x: double) = x * x * x;

integral(cube_double_, 0.0, 1.0, 0.01);
integral(cube_double_, 0.0, 1.0, 0.001);

/* Exercise 1.29 */
def simpson(f: double => double, a: double, b: double, n: long) = {
   val h = abs(b - a) / n;

    def sum_iter(term: double => double, start: long, next: long => long, stop: long, acc: double): double =
       if (start > stop)
          acc;
          else sum_iter(term, next(start), next, stop, acc + term(a + start * h));

   h * sum_iter(f, 1, inc_, n, 0.0);
}

simpson(cube_double_, 0.0, 1.0, 100);

/* Exercise 1.30 */
def sum_iter(term: long => long, a: long, next: long => long, b: long, acc: long): long =
   if (a > b)
      acc;
      else sum_iter(term, next(a), next, b, acc + term(a));

// 'sum_cubes_2' reimplements 'sum_cubes_' but uses 'sum_iter' in place of 'sum'
def sum_cubes_2(a: long, b: long): long = sum_iter(cube, a, inc, b, 0);

sum_cubes_2(1, 10);

/* Exercise 1.31 */
// a.
def product(term: long => long, a: long, next: long => long, b: long): long =
   if (a > b)
      1;
      else term(a) * product(term, next(a), next, b);

def factorial_3(n: long) = product(identity, 1, inc, n);

// b.
def product_iter(term: long => long, a: long, next: long => long, b: long, acc: long): long =
   if (a > b)
      acc;
      else product_iter(term, next(a), next, b, acc * term(a));

def factorial_4(n: long) = product_iter(identity, 1, inc, n, 1);

/* Exercise 1.32 */
// a.
def accumulate(combiner: (long, long) => long, nullValue: long, term: long => long, a: long, next: long => long, b: long): long =
   if (a > b)
      nullValue;
      else combiner(term(a), accumulate(combiner, nullValue, term, next(a), next, b));

// sum:     accumulate(plus, 0, identity, a, inc, b);
// product: accumulate(times, 1, identity, a, inc, b);

// b.
// NOTE: starting value of 'acc' is 'nullValue'
def accumulate_iter(combiner: (long, long) => long, term: long => long, a: long, next: long => long, b: long, acc: long): long =
   if (a > b)
      acc;
      else accumulate_iter(combiner, term, next(a), next, b, combiner(acc, term(a)));

// sum:     accumulate_iter(plus, identity, a, inc, b, 0);
// product: accumulate_iter(times, identity, a, inc, b, 1);

/* Exercise 1.33 */
def filtered_accumulate(combiner: (long, long) => long, nullValue: long, term: long => long, a: long, next: long => long, b: long, pred: long => boolean): long =
   if (a > b)
      nullValue;
      else if (pred(a))
         combiner(term(a), filtered_accumulate(combiner, nullValue, term, next(a), next, b, pred));
         else filtered_accumulate(combiner, nullValue, term, next(a), next, b, pred);

// a.
filtered_accumulate(plus, 0, square, 1, inc, 5, prime);  // 39

// b. Not sure how to implement this without modifying 'filtered_accumulate' to have 'pred'
//    accept two arguments
// 1.3.2 Formulating Abstractions with Higher-Order Procedures - Constructing Procedures Using Lambda
def pi_sum_2(a: double, b: double) =
  sum_double((x: double) => 1.0 / (x * (x + 2.0)), a, (x: double) => x + 4.0, b);

def integral_(f: double=>double, a: double, b: double, dx: double) =
   sum_double(f, a + (dx / 2.0), (x: double) => x + dx, b) * dx;

def plus4(x: long) = x + 4;

val plus4_1 = (x: long) => x + 4;

((x: long, y: long, z: long) => x + y + square(z))(1, 2, 3);

// Using let
def f_2(x: long, y: long) = {
   def f_helper(a: long, b: long) =
      (x * square(a)) + (y * b) + (a * b);
   f_helper(1 + (x * y), 1 - y)
};

def f_3(x: long, y: long) =
   (((a: long, b: long) => ((x * square(a)) + (y * b) + (a * b)))
      (1 + (x * y), 1 - y));

def f_4(x: long, y: long) = {
   val a = 1 + (x * y);
   val b = 1 - y;
   (x * square(a)) + (y * b) + (a * b);
};

val x_1 = 5;
{
   val x_1 = 3;
   x_1 + (x_1 * 10);
} + x_1;

val x_2 = 2;
{
   val y = x_2 + 2;
   {
      val x_2 = 3;
      x_2 * y;
   };
};

def f_5(x: long, y: long) = {
   val a = 1 + (x * y);
   val b = 1 - y;
   (x + square(a)) + (y * b) + (a * b);
};

/* Exercise 1.34 */
def f_6(g: long=>long) = g(2);
f_6(square);
f_6((z: long) => z * (z + 1));

// 1.3.3 Formulating Abstractions with Higher-Order Procedures - Procedures as General Methods

// Half-interval method
def close_enough(x: double, y: double) =
   (abs_double(x - y) < 0.001);

def positive(x: double) = (x >= 0.0);
def negative(x: double) = !positive(x);

def search(f: double=>double, neg_point: double, pos_point: double): double = {
   val midpoint = average(neg_point, pos_point);
   if (close_enough(neg_point, pos_point))
      midpoint
      else {
         val test_value = f(midpoint);
         if (positive(test_value))
            search(f, neg_point, midpoint)
            else if (negative(test_value)) search(f, midpoint, pos_point)
            else midpoint;
      };
};

def half_interval_method(f: double=>double, a: double, b: double) = {
   val a_value = f(a);
   val b_value = f(b);
   if (negative(a_value) && positive(b_value))
      search(f, a, b)
      else if (negative(b_value) && positive(a_value)) search(f, b, a)
      else throw new Exception("Values are not of opposite sign" + a + " " + b);
};

half_interval_method(Math.sin, 2.0, 4.0);

half_interval_method((x: double) => (x * x * x) - (2.0 * x) - 3.0, 1.0, 2.0);

// Fixed points
val tolerance = 0.00001;

def fixed_point(f: double=>double, first_guess: double) = {
   def close_enough(v1: double, v2: double) =
      abs_double(v1 - v2) < tolerance;
   def try_(guess: double): double = {
      val next = f(guess);
      if (close_enough(guess, next))
         next
         else try_(next);
   }
   try_(first_guess);
};

fixed_point(Math.cos, 1.0);

fixed_point((y: double) => Math.sin(y) + Math.cos(y), 1.0);

def sqrt_4(x: double) =
   fixed_point((y : double) => x / y, 1.0)

def sqrt_5(x: double) =
   fixed_point((y: double) => average(y, x / y), 1.0);

/* Exercise 1.35 */
def golden_ratio() =
   fixed_point((x: double) => 1.0 + 1.0 / x, 1.0);

/* Exercise 1.36 */
// Add the following line to function, 'fixed-point':
//  ... val next = f(guess);
//  System.out.println(next);
//  ... if (close_enough(guess, next))

fixed_point((x: double) => Math.log(1000.0) / Math.log(x), 1.5);
fixed_point(average_damp((x: double) => Math.log(1000.0) / Math.log(x)), 1.5);

/* Exercise 1.37 */
/* exercise left to reader to define cont_frac
   cont_frac((i: double) => 1.0, (i: double) => 1.0, k);
*/

/* Exercise 1.38 - unfinished */

/* Exercise 1.39 - unfinished */

// 1.3.4 Formulating Abstractions with Higher-Order Procedures - Procedures as Returned Values
def average_damp(f: double=>double) =
   (x: double) => average(x, f(x));

(average_damp(square_double))(10.0);

def sqrt_6(x: double) =
   fixed_point(average_damp((y: double) => x / y), 1.0);

def cube_root(x: double) =
   fixed_point(average_damp((y: double) => x / square_double(y)), 1.0);

// Newton's method
val dx = 0.00001;
def deriv(g: double=>double) =
   (x: double) => (g(x + dx) - g(x)) / dx;

def cube_1(x: double) = x * x * x;

def newton_transform(g: double=>double) =
   (x: double) => x - (g(x) / (deriv(g)(x)));

def newtons_method(g: double=>double, guess: double) =
   fixed_point(newton_transform(g), guess);

def sqrt_7(x: double) =
   newtons_method((y: double) => square_double(y) - x, 1.0);

// Fixed point of transformed function
def fixed_point_of_transform(g: double=>double, transform: (double=>double)=>(double=>double), guess: double) =
   fixed_point(transform(g), guess);

def sqrt_8(x: double) =
   fixed_point_of_transform((y: double) => x / y, average_damp, 1.0);

def sqrt_9(x: double) =
   fixed_point_of_transform((y: double) => square_double(y) - x,  newton_transform, 1.0);

/* Exercise 1.40 */
def cubic(a: double, b: double, c: double): (double => double) =
   (x: double) => x * x * x + a * x * x + b * x + c;

newtons_method(cubic(5.0, 3.0, 2.5), 1.0); // -4.452...

/* Exercise 1.41 */
def double_(f: long => long) =
   (x: long) => f(f(x));

(double_(inc))(5);                         //  7
(double_(double_(inc)))(5);                //  9
(double_(double_(double_(inc))))(5);       // 13

/* Exercise 1.42 */
def compose_(f: long => long, g: long => long) =
   (x: long) => f(g(x));

(compose_(square, inc))(6);                // 49

/* Exercise 1.43 */
def repeated(f: long => long, n: long) = {
   def iterate(arg: long, i: long): long =
      if (i > n)
         arg;
         else iterate(f(arg), i + 1);

   (x: long) => iterate(x, 1);
}

(repeated(square, 2))(5);                  // 625

/* Exercise 1.44 ('n-fold-smooth' not implemented) */
def smooth(f: double => double, dx: double) =
   (x: double) => average(x, (f(x - dx) + f(x) + f(x + dx)) / 3.0);

fixed_point(smooth((x: double) => Math.log(1000.0) / Math.log(x), 0.05), 1.5);

/* Exercise 1.45 - unfinished */

/* Exercise 1.46 ('sqrt' not implemented) */
def iterative_improve(good_enough: (double, double) => boolean, improve: double => double) = {
   def iterate(guess: double): double = {
      val next = improve(guess);
      if (good_enough(guess, next))
         next;
         else iterate(next);
   }

   (x: double) => iterate(x);
}

def fixed_point_(f: double => double, first_guess: double) = {
   val tolerance = 0.00001;

   def good_enough(v1: double, v2: double) =
      abs_double(v1 - v2) < tolerance;

   (iterative_improve(good_enough, f))(first_guess);
}

fixed_point_(average_damp((x: double) => Math.log(1000.0) / Math.log(x)), 1.5);

eof
   }
}
