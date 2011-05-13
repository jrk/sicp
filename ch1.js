// <html>
// <head><title>SICP Chapter #1</title></head>
// <body>
// <script language='JavaScript' type='text/javascript'>
function print(s) { document.writeln(s + "<br>"); }

// 1.1.1 The Elements of Programming - Expressions
print (486);
print (137 + 349);
print (1000 - 334);
print (5 * 99);
print (10 / 5);
print (2.7 + 10.0);
print (21 + 35 + 12 + 7);
print (25 * 4 * 12);
print ((3 * 5) + (10 - 6));
print ((3 * ((2 * 4) + (3 + 5))) + ((10 - 7) + 6));

// 1.1.2 The Elements of Programming - Naming and the Environment
var size = 2;
print (size);
print (5 * size);
var pi = 3.14159;
var radius = 10.0;
print (pi * radius * radius);
var circumference = 2.0 * pi * radius;
print (circumference);

// 1.1.3 The Elements of Programming - Evaluating Combinations
print ((2 + (4 * 6)) * (3 + 5 + 7));

// 1.1.4 The Elements of Programming - Compound Procedures
function square(x) { return x * x; }
print (square(21));
print (square(2 + 5));
print (square(square(3)));
function sum_of_squares(x, y) { return square(x) + square(y); }
print (sum_of_squares(3, 4));
function f(a) { return sum_of_squares(a + 1, a * 2); }
print (f(5));

// 1.1.5 The Elements of Programming - The Substitution Model for Procedure Application
print (f(5));
print (sum_of_squares(5 + 1, 5 * 2));
print (square(6) + square(10));
print ((6 * 6) + (10 * 10));
print (36 + 100);
print (f(5));
print (sum_of_squares(5 + 1, 5 * 2));
print (square(5 + 1) + square(5 * 2));

print (((5 + 1) * (5 + 1)) + ((5 * 2) * (5 * 2)));
print ((6 * 6) + (10 * 10));
print (36 + 100);
print (136);

// 1.1.6 The Elements of Programming - Conditional Expressions and Predicates
function abs(x) {
   if (x > 0) return x;
   else if (x == 0) return 0;
   else return -x;
}
function abs_1(x) {
   if (x < 0) return -x;
   else return x;
}
var x = 6;
print ((x > 5) && (x < 10));
function ge(x, y) {
   return (x > y) || (x == y)
}
function ge_1(x, y) {
   return !(x < y)
}

// Exercise 1.1
print (10);
print (5 + 3 + 4);
print (9 - 1);
print (6 / 2);
print ((2 * 4) + (4 - 6));
var a = 3;
var b = a + 1;
print (a + b + (a * b));
print (a == b);
print (((b > a) && (b < (a *b))) ? b :a);
print ((a == 4) ? 6 : (b == 4) ? (6 + 7 + a) : 25);
print (2 + ((b > a) ? b : a));
print (((a > b) ? a : (a < b) ? b : -1) * (a + 1));

// Exercise 1.2
print (((5.0 + 4.0 + (2.0 - (3.0 - (6.0 + (4.0 / 5.0))))) /
         (3.0 * (6.0 - 2.0) * (2.0 - 7.0))));

// Exercise 1.4
function a_plus_abs_b(a, b) {
   if (b > 0) return a + b;
   else return a - b;
}

// Exercise 1.5
function p() { return p(); }
function test(x, y) {
   if (x == 0) return 0;
   else return y;
}
// commented out as this is in infinite loop
// test(0, p())

// 1.1.7 The Elements of Programming - Example: Square Roots by Newton's Method
function square_1(x) { return x * x; }

function good_enough(guess, x) {
   return abs(square_1(guess) - x) < 0.001;
}

function average(x, y) {
   return (x + y) / 2.0;
}

function improve(guess, x) {
   return average(guess, parseFloat(x) / guess);
}

function sqrt_iter(guess, x) {
   if (good_enough(guess, x)) return guess;
   else return sqrt_iter(improve(guess, x), x);
}

function sqrt(x) {
   return sqrt_iter(1.0, parseFloat(x));
}

print (sqrt(9));
print (sqrt(100 + 37));
print (sqrt(sqrt(2)+sqrt(3)));
print (square_1(sqrt(1000)));

// Exercise 1.6
function new_if(predicate, then_clause, else_clause) {
   if (predicate) return then_clause;
   else return else_clause;
}
print (new_if((2==3), 0, 5));
print (new_if((1==1), 0, 5));
function sqrt_iter_0(guess, x) {
   return new_if(
      good_enough(guess, x),
      guess,
      sqrt_iter_0(improve(guess, x), x));
}

// 1.1.8 The Elements of Programming - Procedures as Black-Box Abstractions
function square_2(x) { return x * x; }

function double(x) { return x + x; }

function square_3(x) { return Math.exp(double(Math.log(x))); }

function good_enough_1(guess, x) {
   return abs(square_2(guess) - x) < 0.001;
}

function improve_1(guess, x) {
   return average(guess, parseFloat(x) / guess);
}

function sqrt_iter_1(guess, x) {
   if (good_enough_1(guess, x)) return guess;
   else return sqrt_iter_1(improve_1(guess, x), x);
}

function sqrt_1(x) {
   return sqrt_iter_1(1.0, x);
}

print (square_2(5));

// Block-structured
function sqrt_2(x) {
   function good_enough(guess, x) {
      return abs(square(guess) - x) < 0.001;
   }

   function improve(guess, x) {
      return average(guess, float(x) / guess);
   }

   function sqrt_iter(guess, x) {
      if (good_enough(guess, x)) return guess;
      else return sqrt_iter(improve(guess, x), x);
   }

   return sqrt_iter(1.0, x);
}

// Taking advantage of lexical scoping
function sqrt_3(x) {
   function good_enough(guess) {
      return abs(square(guess) - x) < 0.001;
   }

   function improve(guess) {
      return average(guess, parseFloat(x) / guess);
   }

   function sqrt_iter(guess) {
      if (good_enough(guess)) return guess;
      else return sqrt_iter(improve(guess));
   }

   return sqrt_iter(1.0)
}

// 1.2.1 Procedures and the Processes They Generate - Linear Recursion and Iteration

// Recursive
function factorial(n) {
   if (n == 1) return 1;
   else return n * factorial(n - 1);
}

// Iterative
function fact_iter(product, counter, max_count) {
   if (counter > max_count) return product;
   else return fact_iter(counter * product, counter + 1, max_count);
}

function factorial_1(n) {
   return fact_iter(1, 1, n);
}

// Iterative, block-structured (from footnote)
function factorial_2(n) {
   function iter(product, counter) {
      if (counter > n) return product;
      else return iter(counter * product, counter + 1);
   }
   return iter(1, 1);
}

// Exercise 1.9
function inc(a) { return a + 1; }
function dec(a) { return a - 1; }
function plus(a, b) {
   if (a == 0) return b;
   else return inc(dec(a) + b);
}
function plus_1(a, b) {
   if (a == 0) return b;
   else dec(a) + inc(b);
}

// Exercise 1.10
function ax(x, y) {
   if (y == 0) return 0;
   else if (x == 0) return 2 * y;
   else if (y == 1) return 2;
   else return ax(x - 1, ax(x, y - 1));
}
print (ax(1, 10));
print (ax(2, 4));
print (ax(3, 3));
function fx(n) { return ax(0, n); }
function g(n) { return ax(1, n); }
function h(n) { return ax(2, n); }
function k(n) { return 5 * n * n; }

// Exercise 1.11
function fi(n) {
   if (n < 3)
      return n;
      else return fi(n - 1) + 2 * fi(n - 2) + 3 * fi(n - 3);
}

function fi_iter(a, b, c, count) {
   if (count == 0)
      return c;
      else return fi_iter(a + 2 * b + 3 * c, a, b, count - 1);
}

function fi_1(n) { return fi_iter(2, 1, 0, n); }

// Exercise 1.12
function pascals_triangle(n, k) {
   if (n == 0 || k == 0 || n == k)
      return 1;
      else return pascals_triangle(n - 1, k - 1) + pascals_triangle(n - 1, k);
}

// 1.2.2 Procedures and the Processes They Generate - Tree Recursion

// Recursive
function fib(n) {
   if (n == 0) return 0;
   else if (n == 1) return 1;
   else return fib(n - 1) + fib(n - 2);
}

// Iterative
function fib_iter(a, b, count) {
   if (count == 0) return b;
   else return fib_iter(a + b, a, count - 1);
}
function fib_1(n) {
   return fib_iter(1, 0, n);
}

// Counting change
function first_denomination(x) {
   if (x == 1) return 1;
   else if (x == 2) return 5;
   else if (x == 3) return 10;
   else if (x == 4) return 25;
   else if (x == 5) return 50;
}

function cc(amount, kinds_of_coins) {
   if (amount == 0) return 1;
   else if (amount < 0) return 0;
   else if (kinds_of_coins == 0) return 0;
   else return (cc(amount, kinds_of_coins - 1) +
                cc(amount - first_denomination(kinds_of_coins), kinds_of_coins));
}

function count_change(amount) {
   return cc(amount, 5);
}

print (count_change(100));

// 1.2.3 Procedures and the Processes They Generate - Orders of Growth

// Exercise 1.15
function cube(x) { return x * x * x; }

function p(x) { return (3.0 * x) - (4.0 * cube(x)); }
function sine(angle) {
   if (!(abs(angle) > 0.1)) return angle;
   else return p(sine(angle / 3.0));
}

// 1.2.4 Procedures and the Processes They Generate - Exponentiation

// Linear recursion
function expt(b, n) {
   if (n == 0) return 1;
   else return b * expt(b, (n - 1));
}

// Linear iteration
function expt_iter(b, counter, product) {
   if (counter == 0) return product;
   else return expt_iter(b, counter - 1, b * product);
}
function expt_1(b, n) {
   return expt_iter(b, n, 1);
}

// Logarithmic iteration
function even(n) { return ((n % 2) == 0); }

function fast_expt(b, n) {
   if (n == 0) return 1;
   else
      if (even(n)) return square(fast_expt(b, n / 2));
      else return b * fast_expt(b, n - 1);
}

// Exercise 1.17
function multiply(a, b) {
   if (b == 0) return 0;
   else return a + (a * (b - 1));
}

// Exercise 1.19
// exercise left to reader to solve for p' and q'
// function fib_iter(a, b, p, q, count) {
//    if (count == 0) return b;
//    else
//       if (even(count)) return fib_iter(a, b, p', q', count / 2);
//       else fib_iter((b * q) + (a * q) + (a * p), (b * p) + (a * q), p, q, count - 1);
// }
// function fib(n) {
//    return fib_iter(1, 0, 0, 1, n);
// }


// 1.2.5 Procedures and the Processes They Generate - Greatest Common Divisors
function gcd(a, b) {
   if (b == 0) return a;
   else return gcd(b, a % b);
}

// 1.2.6 Procedures and the Processes They Generate - Example: Testing for Primality

// prime
function divides(a, b) { return ((b % a) == 0); }

function find_divisor(n, test_divisor) {
   if (square(test_divisor) > n) return n;
   else if (divides(test_divisor, n)) return test_divisor;
   else return find_divisor(n, test_divisor + 1);
}

function smallest_divisor(n) { return find_divisor(n, 2); }

function prime(n) { return (n == smallest_divisor(n)); }

// fast_prime
function expmod(nbase, nexp, m) {
   if (nexp == 0) return 1;
   else
      if (even(nexp)) return square(expmod(nbase, nexp / 2, m)) % m;
      else return (nbase * (expmod(nbase, (nexp - 1), m))) % m;
}

function fermat_test(n) {
   function try_it(a) { return (expmod(a, n, n) == a); }
   return try_it(1 + random.int(0, n-1));
}

function fast_prime(n, ntimes) {
   if (ntimes == 0) return true;
   else
      if (fermat_test(n)) return fast_prime(n, ntimes - 1);
      else return false;
}

// Exercise 1.22
function report_prime(elapsed_time) {
   print (" *** " + elapsed_time);
}
function start_prime_test(n, start_time) {
   if (prime(n)) {
      report_prime((new Date()).getTime() - start_time);
   }
}
function timed_prime_test(n) {
   print ("\n" + n);
   start_prime_test(n, (new Date()).getTime());
}

// Exercise 1.25
function expmod_1(nbase, nexp, m) {
   return fast_expt(nbase, nexp) % m;
}

// Exercise 1.26
function expmod_2(nbase, nexp, m) {
   if (nexp == 0) return 1;
   else
      if (even(nexp)) return (expmod_2(nbase, (nexp / 2), m) *
                              expmod_2(nbase, (nexp / 2), m)) % m;
      else return (nbase * expmod_2(nbase, nexp - 1, m)) % m;
}

// 1.3 Formulating Abstractions with Higher-Order Procedures
function cube_1(x) { return x * x * x; }

// 1.3.1 Formulating Abstractions with Higher-Order Procedures - Procedures as Arguments
function sum_integers(a, b) {
   if (a > b) return 0;
   else return a + sum_integers(a + 1, b);
}

function sum_cubes(a, b) {
   if (a > b) return 0;
   else return cube(a) + sum_cubes(a + 1, b);
}

function pi_sum(a, b) {
   if (a > b) return 0.0;
   else return (1.0 / (a * (a + 2.0))) + pi_sum(a + 4.0, b);
}

function sum(term, a, next, b) {
   if (a > b) return 0;
   else return term(a) + sum(term, next(a), next, b);
}

// Using sum
function inc_1(n) { return n + 1; }

function sum_cubes_1(a, b) {
   return sum(cube_1, a, inc, b);
}

print (sum_cubes_1(1, 10));

function identity(x) { return x; }

function sum_integers_1(a, b) {
   return sum(identity, a, inc_1, b);
}

print (sum_integers_1(1, 10));

function pi_sum_1(a, b) {
   function pi_term(x) { return 1.0 / (x * (x + 2.0)); }
   function pi_next(x) { return x + 4.0; }
   return sum(pi_term, a, pi_next, b);
}

print (8.0 * pi_sum_1(1, 1000));

function integral(f, a, b, dx) {
   function add_dx(x) { return x + dx; }
   return sum(f, a + (dx / 2.0), add_dx, b) * dx;
}

function cube(x) { return x * x * x; }

print (integral(cube, 0.0, 1.0, 0.01));
// exceeds maximum recursion depth
// print (integral(cube, 0.0, 1.0, 0.001));

// Exercise 1.29
function simpson(f, a, b, n) {
   var h = abs(b - a) / n;

   function sum_iter(term, start, next, stop, acc) {
      if (start > stop)
         return acc;
         else return sum_iter(term, next(start), next, stop, acc + term(a + start * h));
   }

   return h * sum_iter(f, 1, inc, n, 0.0);
}

simpson(cube_1, 0.0, 1.0, 100);

// Exercise 1.30
function sum_iter(term, a, next, b, acc) {
   if (a > b)
      return acc;
      else return sum_iter(term, next(a), next, b, acc + term(a));
}

// 'sum_cubes_2' reimplements 'sum_cubes_' but uses 'sum_iter' in place of 'sum'
function sum_cubes_2(a, b) { return sum_iter(cube, a, inc, b, 0); }

sum_cubes_2(1, 10);

// Exercise 1.31
// a.
function product(term, a, next, b) {
   if (a > b)
      return 1;
      else return term(a) * product(term, next(a), next, b);
}

function factorial_2(n) { return product(identity, 1, inc, n); }

// b.
function product_iter(term, a, next, b, acc) {
   if (a > b)
      return acc;
      else return product_iter(term, next(a), next, b, acc * term(a));
}

function factorial_3(n) { return product_iter(identity, 1, inc, n, 1); }

// Exercise 1.32
// a.
function accumulate(combiner, nullValue, term, a, next, b) {
   if (a > b)
      return nullValue;
      else return combiner(term(a), accumulate(combiner, nullValue, term, next(a), next, b));
}

// sum:     accumulate(plus, 0, identity, a, inc, b);
//
// function times(a, b) { return a * b; }
// product: accumulate(times, 1, identity, a, inc, b);

// b.
// NOTE: starting value of 'acc' is 'nullValue'
function accumulate_iter(combiner, term, a, next, b, acc) {
   if (a > b)
      return acc;
      else return accumulate_iter(combiner, term, next(a), next, b, combiner(acc, term(a)));
}

// sum:     accumulate_iter(plus, identity, a, inc, b, 0);
//
// function times(a, b) { return a * b; }
// product: accumulate_iter(times, identity, a, inc, b, 1);

// Exercise 1.33
function filtered_accumulate(combiner, nullValue, term, a, next, b, pred) {
   if (a > b)
      return nullValue;
      else if (pred(a))
         return combiner(term(a), filtered_accumulate(combiner, nullValue, term, next(a), next, b, pred));
         else return filtered_accumulate(combiner, nullValue, term, next(a), next, b, pred);
}

// a.
filtered_accumulate(plus, 0, square, 1, inc, 5, prime);  // 39

// b. Not sure how to implement this without modifying 'filtered_accumulate' to have 'pred'
//    accept two arguments

// 1.3.2 Formulating Abstractions with Higher-Order Procedures - Constructing Procedures Using Lambda
function pi_sum_2(a, b) {
   return sum(
      function(x) { return 1.0 / (x * (x + 2.0)); },
      a,
      function(x) { return x + 4.0 },
      b);
}

function integral_1(f, a, b, dx) {
   return sum(f, a + (dx / 2.0), function(x) { return x + dx; }, b) * dx;
}

function plus4(x) { return x + 4; }

plus4_1 = function(x) { return x + 4; }

print ((function(x, y, z) { return x + y + square(z) }) (1, 2, 3));

// Using let
function f_1(x, y) {
   function f_helper(a, b) {
      return (x * square(a)) + (y * b) + (a * b);
   }
   return f_helper(1 + (x * y), 1 - y)
}

function f_2(x, y) {
   return (function(a, b) { return (x * square(a)) + (y * b) + (a * b); }) (1 + (x * y), 1 - y);
}

function f_3(x, y) {
   a = 1 + (x * y);
   b = 1 - y;
   return (x * square(a)) + (y * b) + (a * b);
}

// javascript does not have let binding - used lambda to emulate
var x = 5;
print (function() {
         var x = 3;
         return x + (x * 10);
       }() + x);

var x = 2;
print (function(x) {
         var y = x + 2;
         var x = 3;
         return x * y;
       }(x));

function f_4(x, y) {
   a = 1 + (x * y);
   b = 1 - y;
   return (x + square(a)) + (y * b) + (a * b);
}

// Exercise 1.34
function f_5(g) { return g(2); }
print (f_5(square));
print (f_5(function(z) { return z * (z + 1) } ));

// 1.3.3 Formulating Abstractions with Higher-Order Procedures - Procedures as General Methods

// Half-interval method
function close_enough(x, y) {
   return (abs(x - y) < 0.001);
}

function positive(x) { return (x >= 0.0); }
function negative(x) { return !(positive(x)); }

function search(f, neg_point, pos_point) {
   midpoint = average(neg_point, pos_point);
   if (close_enough(neg_point, pos_point)) return midpoint;
   else
      test_value = f(midpoint);
      if (positive(test_value)) return search(f, neg_point, midpoint);
      else if (negative(test_value)) return search(f, midpoint, pos_point);
      else return midpoint;
}

function half_interval_method(f, a, b) {
   a_value = f(a);
   b_value = f(b);
   if (negative(a_value) && positive(b_value)) return search(f, a, b);
   else if (negative(b_value) && positive(a_value)) return search(f, b, a);
   else throw ("Exception: Values are not of opposite sign " + a + " " + b);
}

print (half_interval_method(Math.sin, 2.0, 4.0));

print (half_interval_method(function(x) { return (x * x * x) - (2.0 * x) - 3.0; }, 1.0, 2.0));

// Fixed points
tolerance = 0.00001

function fixed_point(f, first_guess) {
   function close_enough(v1, v2) {
      return abs(v1 - v2) < tolerance;
   }
   function tryit(guess) {
      next = f(guess);
      if (close_enough(guess, next)) return next;
      else return tryit(next);
   }
   return tryit(first_guess);
}

print (fixed_point(Math.cos, 1.0));

print (fixed_point(function(y) { return Math.sin(y) + Math.cos(y); }, 1.0));

// note: this function does not converge
function sqrt_4(x) {
   return fixed_point(function(y) { return parseFloat(x) / y; }, 1.0)
}

function sqrt_5(x) {
   return fixed_point(function(y) { return average(y, parseFloat(x) / y); }, 1.0)
}

// Exercise 1.35
function golden_ratio() {
   return fixed_point(function(x) { return 1.0 + 1.0 / x; }, 1.0);
}

// Exercise 1.36
// Add the following line to function, 'fixed_point':
//  ... var next = f(guess);
//  print(next);
//  ... if (close_enough(guess, next))

print(fixed_point(function(x) { return Math.log(1000.0) / Math.log(x); }, 1.5));

// Exercise 1.37
// exercise left to reader to define cont_frac
// cont_frac(function(i) { return 1.0; }, function(i) { return 1.0; }, k)

// Exercise 1.38 - unfinished

// Exercise 1.39 - unfinished

// 1.3.4 Formulating Abstractions with Higher-Order Procedures - Procedures as Returned Values
function average_damp(f) {
   return (function(x) { return average(parseFloat(x), f(x)); } )
}

print ((average_damp(square)) (10.0));

// Exercise 1.36 continued ...
print(fixed_point(average_damp(function(x) { return Math.log(1000.0) / Math.log(x); }), 1.5));

function sqrt_6(x) {
   return fixed_point(average_damp(function(y) { return parseFloat(x) / y; }), 1.0);
}

function cube_root(x) {
   return fixed_point(average_damp(function(y) { return parseFloat(x) / square(y); }), 1.0)
}

print (cube_root(8));

// Newton's method
dx = 0.00001
function deriv(g) {
   return (function(x){ return parseFloat(g(x + dx) - g(x)) / dx; });
}

function cube_2(x) { return x * x * x; }

print (deriv(cube_2) (5.0));

function newton_transform(g) {
   return (function(x) { return x - (parseFloat(g(x)) / (deriv(g) (x))); });
}

function newtons_method(g, guess) {
   return fixed_point(newton_transform(g), guess);
}

function sqrt_7(x) {
   return newtons_method(function(y) { return square(y) - x; } , 1.0);
}

// Fixed point of transformed function
function fixed_point_of_transform(g, transform, guess) {
   return fixed_point(transform(g), guess);
}

function sqrt_8(x) {
   return fixed_point_of_transform(function(y) { return x / y; }, average_damp, 1.0);
}

function sqrt_9(x) {
   return fixed_point_of_transform(function(y) { return square(y) - x; }, newton_transform, 1.0)
}

// Exercise 1.40
function cubic(a, b, c) {
   return function(x) { return x * x * x + a * x * x + b * x + c; };
}

print(newtons_method(cubic(5.0, 3.0, 2.5), 1.0)); // -4.452...

// Exercise 1.41
function double_(f) {
   return function(x) { return f(f(x)); };
}

print((double_(inc))(5));                         //  7
print((double_(double_(inc)))(5));                //  9
print((double_(double_(double_(inc))))(5));       // 13

// Exercise 1.42
function compose_(f, g) {
   return function(x) { return f(g(x)); };
}

print((compose_(square, inc))(6));                // 49

// Exercise 1.43
function repeated(f, n) {
   function iterate(arg, i) {
      if (i > n)
         return arg;
         else return iterate(f(arg), i + 1);
   }

   return function(x) { return iterate(x, 1); };
}

print((repeated(square, 2))(5));                  // 625

// Exercise 1.44 ('n-fold-smooth' not implemented)
function smooth(f, dx) {
   return function(x) { return average(x, (f(x - dx) + f(x) + f(x + dx)) / 3.0); };
}

print(fixed_point(smooth(function(x) { return Math.log(1000.0) / Math.log(x); }, 0.05), 1.5));

// Exercise 1.45 - unfinished

// Exercise 1.46 ('sqrt' not implemented)
function iterative_improve(good_enough, improve) {
   function iterate_(guess) {
      var next = improve(guess);
      if (good_enough(guess, next))
         return next;
         else return iterate_(next);
   }

   return function(x) { return iterate_(x); };
}

function fixed_point_(f, first_guess) {
   var tolerance = 0.00001;

   function good_enough(v1, v2) {
      return Math.abs(v1 - v2) < tolerance;
   }

   return (iterative_improve(good_enough, f))(first_guess);
}

print(fixed_point_(average_damp(function(x) { return Math.log(1000.0) / Math.log(x); }), 1.5));

// Note:  Must be careful using lexical scoping in JavaScript.
// The following will print "after"
var vx = "before";
function fv() { return vx; }
var vx = "after";
print (fv());

// The following will print "second" two times
function fg() { return "first"; }
print(fg());
function fg() { return "second"; }
print(fg());

// </script>
// </body>
// </html>
