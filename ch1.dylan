// 1.1.1 The Elements of Programming - Expressions
486;
137 + 349;
1000 - 334;
5 * 99;
floor/(10, 5);
2.7 + 10.0;
21 + 35 + 12 + 7;
25 * 4 * 12;
(3 * 5) + (10 - 6);
(3 * ((2 * 4) + (3 + 5))) + ((10 - 7) + 6);
// 1.1.2 The Elements of Programming - Naming and the Environment
let size = 2;
size;
5 * size;
let pi = 3.14159;
let radius = 10.0;
pi * radius * radius;
let circumference = 2.0 * pi * radius;
circumference;
// 1.1.3 The Elements of Programming - Evaluating Combinations
(2 + (4 * 6)) * (3 + 5 + 7);
// 1.1.4 The Elements of Programming - Compound Procedures
define function square(x :: <integer>) => result :: <integer>;
  x * x;
end function square;

square(21);
square(2 + 5);
square(square(3));

define function sum_of_squares(x :: <integer>, y :: <integer>) => result :: <integer>;
  square(x) + square(y);
end function sum_of_squares;

sum_of_squares(3, 4);

define function f(a :: <integer>) => result :: <integer>;
  sum_of_squares(a + 1, a * 2);
end function f;

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
define function abs_(x :: <integer>) => result :: <integer>;
  if (x > 0)
    x
  elseif (x == 0)
    0
  else
    -x
  end if;
end function abs_;

define function abs_1(x :: <integer>) => result :: <integer>;
  if (x < 0)
    -x
  else
    x
  end if;
end function abs_1;

define function ge(x :: <integer>, y :: <integer>) => result :: <boolean>;
  (x > y) | (x == y);
end function ge;

define function ge_1(x :: <integer>, y :: <integer>) => result :: <boolean>;
  ~(x < y);
end function ge_1;

let x = 6;
(x > 5) & (x < 10);

/* Exercise 1.1 */
10;
5 + 3 + 4;
9 - 1;
floor/(6, 2);
(2 * 4) + (4 - 6);
let a = 3;
let b = a + 1;
a + b + (a * b);
(a == b);

if ((b > a) & (b < (a * b)))
  b
else
  a
end if;

if (a == 4)
  6
elseif (b == 4)
  6 + 7 + a
else
  25
end if;

2 + (((b > a) & b) | a);
(((a > b) & a) | ((a < b) & b) | -1) * (a + 1);

/* Exercise 1.2 */
(5.0 + 4.0 + (2.0 - (3.0 - (6.0 + (4.0 / 5.0))))) / (3.0 * (6.0 - 2.0) * (2.0 - 7.0));

/* Exercise 1.3 */
define function three_n(n1 :: <integer>, n2 :: <integer>, n3 :: <integer>) => result :: <integer>;
  if (n1 > n2)
    if (n1 > n3)
      if (n2 > n3)
        n1 * n1 + n2 * n2
      else
        n1 * n1 + n3 * n3
      end if;
    else
      n1 * n1 + n3 * n3
    end if;
  else
    if (n2 > n3)
      if (n1 > n3)
        n2 * n2 + n1 * n1
      else
        n2 * n2 + n3 * n3
      end if;
    else
      n2 * n2 + n3 * n3
    end if;
  end if;
end function three_n;

/* Exercise 1.4 */
define function a_plus_abs_b(a :: <integer>, b :: <integer>) => result :: <integer>;
  ((b > 0) & (a + b)) | (a - b);
end function a_plus_abs_b;

/* Exercise 1.5 */
define function p() => result :: <integer>;
  p();
end function p;

define function test(x :: <integer>, y :: <integer>) => result :: <integer>;
  if (zero?(x))
    0
  else
    y
  end if;
end function test;

test(0, p());
// 1.1.7 The Elements of Programming - Example: Square Roots by Newton's Method
define function square_double(x :: <double-float>) => result :: <double-float>;
  x * x;
end function square_double;

define function abs_double(x :: <double-float>) => result :: <double-float>;
  ((x < 0.0d0) & -x) | x;
end function abs_double;

define function good_enough(guess :: <double-float>, x :: <double-float>) => result :: <boolean>;
  abs_double(square_double(guess) - x) < 0.001d0;
end function good_enough;

define function average(x :: <double-float>, y :: <double-float>) => result :: <double-float>;
  (x + y) / 2.0d0;
end function average;

define function improve(guess :: <double-float>, x :: <double-float>) => result :: <double-float>;
  average(guess, x / guess);
end function improve;

define function sqrt_iter(guess :: <double-float>, x :: <double-float>) => result :: <double-float>;
  if (good_enough(guess, x))
    guess
  else
    sqrt_iter(improve(guess, x), x)
  end if;
end function sqrt_iter;

define function sqrt(x :: <double-float>) => result :: <double-float>;
  sqrt_iter(1.0d0, x);
end function sqrt;

sqrt(9.0);
sqrt(100.0 + 37.0);
sqrt((sqrt(2.0))+(sqrt(3.0)));
square_double(sqrt(1000.0));

/* Exercise 1.6 */
define function new_if(predicate :: <boolean>, then_clause :: <double-float>, else_clause :: <double-float>) => result :: <double-float>;
  if (predicate)
    then_clause
  else
    else_clause
  end if;
end function new_if;

new_if((2 == 3), 0.0d0, 5.0d0);
new_if((1 == 1), 0.0d0, 5.0d0);

define function sqrt_iter_(guess :: <double-float>, x :: <double-float>) => result :: <double-float>;
  new_if(
    good_enough(guess, x),
    guess,
    sqrt_iter_(improve(guess, x), x));
end function sqrt_iter_;

/* Exercise 1.7 */
define function good_enough_gp(guess :: <double-float>, prev :: <double-float>) => result :: <boolean>;
  abs_double(guess - prev) / guess < 0.001d0;
end function good_enough_gp;

define function sqrt_iter_gp(guess :: <double-float>, prev :: <double-float>, x :: <double-float>) => result :: <double-float>;
  if (good_enough_gp(guess, prev))
    guess
  else
    sqrt_iter_gp(improve(guess, x), guess, x)
  end if;
end function sqrt_iter_gp;

define function sqrt_gp(x :: <double-float>) => result :: <double-float>;
  sqrt_iter_gp(4.0d0, 1.0d0, x);
end function sqrt_gp;

/* Exercise 1.8 */
define function improve_cube(guess :: <double-float>, x :: <double-float>) => result :: <double-float>;
  (2.0d0 * guess + x / (guess * guess)) / 3.0d0;
end function improve_cube;

define function cube_iter(guess :: <double-float>, prev :: <double-float>, x :: <double-float>) => result :: <double-float>;
  if (good_enough_gp(guess, prev))
    guess
  else
    cube_iter(improve_cube(guess, x), guess, x)
  end if;
end function cube_iter;

define function cube_root(x :: <double-float>) => result :: <double-float>;
  cube_iter(27.0d0, 1.0d0, x);
end function cube_root;
// 1.1.8 The Elements of Programming - Procedures as Black-Box Abstractions
define function square_double_1(x :: <double-float>) => result :: <double-float>;
  x * x;
end function square_double_1;

define function doublex(x :: <double-float>) => result :: <double-float>;
  x + x;
end function doublex;

define function square_double_2(x :: <double-float>) => result :: <double-float>;
  exp(doublex(log(x)));
end function square_double_2;

define function good_enough_1(guess :: <double-float>, x :: <double-float>) => result :: <boolean>;
  abs_double(square_double_2(guess) - x) < 0.001d0;
end function good_enough_1;

define function improve_1(guess :: <double-float>, x :: <double-float>) => result :: <double-float>;
  average(guess, x / guess);
end function improve_1;

define function sqrt_iter_1(guess :: <double-float>, x :: <double-float>) => result :: <double-float>;
  if (good_enough_1(guess, x))
    guess
  else
    sqrt_iter_1(improve_1(guess, x), x)
  end if;
end function sqrt_iter_1;

define function sqrt_1(x :: <double-float>) => result :: <double-float>;
  sqrt_iter_1(1.0d0, x);
end function sqrt_1;

square_double_1(5.0);

// Block-structured
define function sqrt_2(x :: <double-float>) => result :: <double-float>;
  local
    method good_enough(guess :: <double-float>, x :: <double-float>) => result :: <boolean>;
      abs_double(square_double(guess) - x) < 0.001d0;
    end,

    method improve(guess :: <double-float>, x :: <double-float>) => result :: <double-float>;
      average(guess, x / guess);
    end,

    method sqrt_iter(guess :: <double-float>, x :: <double-float>) => result :: <double-float>;
      if (good_enough(guess, x))
        guess
      else
        sqrt_iter(improve(guess, x), x)
      end if;
    end;

  sqrt_iter(1.0d0, x);
end function sqrt_2;

// Taking advantage of lexical scoping
define function sqrt_3(x :: <double-float>) => result :: <double-float>;
  local
    method good_enough(guess :: <double-float>) => result :: <boolean>;
      abs_double(square_double(guess) - x) < 0.001d0;
    end,

    method improve(guess :: <double-float>) => result :: <double-float>;
      average(guess, x / guess);
    end,

    method sqrt_iter(guess :: <double-float>) => result :: <double-float>;
      if (good_enough(guess))
        guess
      else
        sqrt_iter(improve(guess))
      end if;
    end;

  sqrt_iter(1.0d0);
end function sqrt_3;
// 1.2.1 Procedures and the Processes They Generate - Linear Recursion and Iteration
// Recursive
define function factorial(n :: <integer>) => result :: <integer>;
  if (n == 1)
    1
  else 
    n * factorial(n - 1)
  end if;
end function factorial;

// Iterative
define function fact_iter(product :: <integer>, counter :: <integer>, max_count :: <integer>) => result :: <integer>;
  if (counter > max_count)
    product
  else
    fact_iter(counter * product, counter + 1, max_count)
  end if;
end function fact_iter;

define function factorial_1(n :: <integer>) => result :: <integer>;
  fact_iter(1, 1, n);
end function factorial_1;

// Iterative, block-structured (from footnote)
define function factorial_2(n :: <integer>) => result :: <integer>;
  local method iter(product :: <integer>, counter :: <integer>) => result :: <integer>;
    if (counter > n)
      product
    else
      iter(counter * product, counter + 1)
    end if;
  end;
  
  iter(1, 1);
end function factorial_2;

/* Exercise 1.9 */
define function inc(a :: <integer>) => result :: <integer>;
  a + 1;
end function inc;

define function dec(a :: <integer>) => result :: <integer>;
  a - 1;
end function dec;

define function plus(a :: <integer>, b :: <integer>) => result :: <integer>;
  if (a == 0)
    b
  else
    inc(dec(a) + b)
  end if;
end function plus;

define function plus_1(a :: <integer>, b :: <integer>) => result :: <integer>;
  if (a == 0)
    b
  else
    dec(a) + inc(b)
  end if;
end function plus_1;

/* Exercise 1.10 */
define function a_(x :: <integer>, y :: <integer>) => result :: <integer>;
  case
    (y == 0)  => 0;
    (x == 0)  => 2 * y;
    (y == 1)  => 2;
    otherwise => a_(x - 1, a_(x, y - 1));
  end case;
end function a_;

a_(1, 10);
a_(2, 4);
a_(3, 3);

define function f_(n :: <integer>) => result :: <integer>;
  a_(0, n);
end function f_;

define function g_(n :: <integer>) => result :: <integer>;
  a_(1, n);
end function g_;

define function h_(n :: <integer>) => result :: <integer>;
  a_(2, n);
end function h_;

define function k_(n :: <integer>) => result :: <integer>;
  5 * n * n;
end function k_;
// 1.2.2 Procedures and the Processes They Generate - Tree Recursion
// Recursive
define function fib(n :: <integer>) => result :: <integer>;
  case
    (n == 0)  => 0;
    (n == 1)  => 1;
    otherwise => fib(n - 1) + fib(n - 2);
  end case;
end function fib;

// Iterative
define function fib_iter(a :: <integer>, b :: <integer>, count :: <integer>) => result :: <integer>;
  if (count == 0)
    b
  else
    fib_iter(a + b, a, count - 1)
  end if;
end function fib_iter;

define function fib_1(n :: <integer>) => result :: <integer>;
  fib_iter(1, 0, n);
end function fib_1;

// Counting change
define function first_denomination(x :: <integer>) => result :: <integer>;
  case
    (x == 1)  => 1;
    (x == 2)  => 5;
    (x == 3)  => 10;
    (x == 4)  => 25;
    (x == 5)  => 50;
    otherwise => error("Domain");
  end case;
end function first_denomination;

define function cc(amount :: <integer>, kinds_of_coins :: <integer>) => result :: <integer>;
  case
    (amount == 0)
      => 1;
    (amount < 0)
      => 0;
    (kinds_of_coins == 0)
      => 0;
    otherwise
      => cc(amount, kinds_of_coins - 1) +
         cc(amount - first_denomination(kinds_of_coins), kinds_of_coins);
  end case;
end function cc;

define function count_change(amount :: <integer>) => result :: <integer>;
  cc(amount, 5);
end function count_change;

count_change(100);

/* Exercise 1.11 */
define function f(n :: <integer>) => result :: <integer>;
  if (n < 3) 
    n
  else
    f(n - 1) + 2 * f(n - 2) + 3 * f(n - 3)    
  end if;
end function f;

define function f_iter(a :: <integer>, b :: <integer>, c :: <integer>, count :: <integer>) => result :: <integer>;
  if (count == 0)
    c
  else
    f_iter(a + 2 * b + 3 * c, a, b, count - 1)
  end if;
end function f_iter;

define function f_1(n :: <integer>) => result :: <integer>;
  f_iter(2, 1, 0, n);
end function f_1;

/* Exercise 1.12 */
define function pascals_triangle(n :: <integer>, k :: <integer>) => result :: <integer>;
  case
    (n == 0)  => 1;
    (k == 0)  => 1;
    (n == k)  => 1;
    otherwise => pascals_triangle(n - 1, k - 1) + pascals_triangle(n - 1, k);
  end case;
end function pascals_triangle;
// 1.2.3 Procedures and the Processes They Generate - Orders of Growth
/* Exercise 1.15 */
define function cube_double(x :: <double-float>) => result :: <double-float>;
  x * x * x;
end function cube_double;

define function p_(x :: <double-float>) => result :: <double-float>;
  (3.0d0 * x) - (4.0d0 * cube_double(x));
end function p_;

define function sine(angle :: <double-float>) => result :: <double-float>;
  if (~(abs_double(angle) > 0.1d0))
    angle
  else
    p_(sine(angle / 3.0d0))
  end if;
end function sine;
// 1.2.4 Procedures and the Processes They Generate - Exponentiation
// Linear recursion
define function expt(b :: <integer>, n :: <integer>) => result :: <integer>;
  if (n == 0)
    1
  else
    b * expt(b, n - 1)
  end if;
end function expt;

// Linear iteration
define function expt_iter(b :: <integer>, counter :: <integer>, product :: <integer>) => result :: <integer>;
  if (counter == 0)
    product
  else
    expt_iter(b, counter - 1, b * product)
  end if;
end function expt_iter;

define function expt_1(b :: <integer>, n :: <integer>) => result :: <integer>;
  expt_iter(b, n, 1);
end function expt_1;

// Logarithmic iteration
define function iseven(n :: <integer>) => result :: <boolean>;
  (modulo(n, 2) == 0);
end function iseven;

define function fast_expt(b :: <integer>, n :: <integer>) => result :: <integer>;
  if (n == 0)
    1
  else
    if (iseven(n))
      (fast_expt(b, floor/(n, 2)) ^ 2)
    else
      b * fast_expt(b, n - 1)
    end if;
  end if;
end function fast_expt;

/* Exercise 1.17 */
define function multiply(a :: <integer>, b :: <integer>) => result :: <integer>;
  if (b == 0)
    0
  else
    a + (a * (b - 1))
  end if;
end function multiply;

/* Exercise 1.19 */
define function fib_iter_(a :: <integer>, b :: <integer>, p :: <integer>, q :: <integer>, count :: <integer>) => result  :: <integer>;
  if (zero?(count))
    b
  elseif (iseven(count))
    fib_iter_(a, b, p * p + q * q, 2 * p * q + q * q, floor/(count, 2))
  else
    fib_iter_(b * q + a * q + a * p, b * p + a * q, p, q, count - 1)      
  end if;
end function fib_iter_;

define function fib_2(n :: <integer>) => result :: <integer>;
  fib_iter_(1, 0, 0, 1, n);
end function fib_2;
// 1.2.5 Procedures and the Processes They Generate - Greatest Common Divisors
define function gcd_(a :: <integer>, b :: <integer>) => result  :: <integer>;
  if (b == 0)
    a
  else
    gcd_(b, modulo(a, b))
  end if;
end function gcd_;
// 1.2.6 Procedures and the Processes They Generate - Example: Testing for Primality
// prime
define function divides(a :: <integer>, b :: <integer>) => result :: <boolean>;
  (modulo(b, a) == 0);
end function divides;

define function find_divisor(n :: <integer>, test_divisor :: <integer>) => result :: <integer>;
  if (square(test_divisor) > n)
    n
  elseif (divides(test_divisor, n))
    test_divisor
  else
    find_divisor(n, test_divisor + 1)
  end if;
end function find_divisor;

define function smallest_divisor(n :: <integer>) => result :: <integer>;
  find_divisor(n, 2);
end function smallest_divisor;

define function prime(n :: <integer>) => result :: <boolean>;
  (n == smallest_divisor(n));
end function prime;

// fast_prime
define function expmod(nbase :: <integer>, nexp :: <integer>, m :: <integer>) => result :: <integer>;
  if (nexp == 0)
    1
  else
    if (iseven(nexp))
      modulo(square(expmod(nbase, floor/(nexp, 2), m)), m)
    else
      modulo(nbase * (expmod(nbase, (nexp - 1), m)), m)
    end if;
  end if;
end function expmod;

define function fermat_test(n :: <integer>) => result :: <boolean>;
  local
    method try_it(a :: <integer>) => result :: <boolean>;
      (expmod(a, n, n) == a);
    end;

  try_it(1 + Random($maximum-integer) * (n - 1));
end function fermat_test;

define function fast_prime(n :: <integer>, ntimes :: <integer>) => result :: <boolean>;
  if (ntimes == 0)
    #t
  else
    if (fermat_test(n))
      fast_prime(n, ntimes - 1)
    else
      #f
    end if;
  end if;
end function fast_prime;

/* Exercise 1.21 */
format-out("%=\n", smallest_divisor(199));    //  199
format-out("%=\n", smallest_divisor(1999));   // 1999
format-out("%=\n", smallest_divisor(19999));  //    7

/* Exercise 1.22 - unfinished */

/* Exercise 1.25 */
define function expmod_(nbase :: <integer>, nexp :: <integer>, m :: <integer>) => result :: <integer>;
  modulo(fast_expt(nbase, nexp), m);
end function expmod_;

/* Exercise 1.26 */
define function expmod_2(nbase :: <integer>, nexp :: <integer>, m :: <integer>) => result :: <integer>;
  if (nexp == 0)
    1
  else
    if (iseven(nexp))
      modulo(expmod_2(nbase, floor/(nexp, 2), m) * expmod_2(nbase, floor/(nexp, 2), m), m)
    else
      modulo(nbase * expmod_2(nbase, nexp - 1, m), m)
    end if;
  end if;
end function expmod_2;
// 1.3 Formulating Abstractions with Higher-Order Procedures
define function cube(x :: <integer>) => result :: <integer>;
  x * x * x;
end function cube;
// 1.3.1 Formulating Abstractions with Higher-Order Procedures - Procedures as Arguments
define function sum_integers(a :: <integer>, b :: <integer>) => result :: <integer>;
  if (a > b)
    0
  else
    a + sum_integers(a + 1, b)
  end if;
end function sum_integers;

define function sum_cubes(a :: <integer>, b :: <integer>) => result :: <integer>;
  if (a > b)
    0
  else
    cube(a) + sum_cubes(a + 1, b)
  end if;
end function sum_cubes;

define function pi_sum(a :: <double-float>, b :: <double-float>) => result :: <double-float>;
  if (a > b)
    0.0d0
  else
    (1.0d0 / (a * (a + 2.0d0))) + pi_sum(a + 4.0d0, b)
  end if;
end function pi_sum;

define function sum(term :: <function>, a :: <integer>, next :: <function>, b :: <integer>) => result :: <integer>;
  if (a > b)
    0
  else
    term(a) + sum(term, next(a), next, b)
  end if;
end function sum;

// Using sum
define function inc_(n :: <integer>) => result :: <integer>;
  n + 1;
end function inc_;

define function sum_cubes_(a :: <integer>, b :: <integer>) => result :: <integer>;
  sum(cube, a, inc_, b);
end function sum_cubes_;

sum_cubes_(1, 10);

define function identity_(x :: <integer>) => result :: <integer>;
  x;
end function identity_;

define function sum_integers_(a :: <integer>, b :: <integer>) => result :: <integer>;
  sum(identity_, a, inc_, b);
end function sum_integers_;

sum_integers_(1, 10);

define function sum_double(term :: <function>, a :: <double-float>, next :: <function>, b :: <double-float>) => result :: <double-float>;
  if (a > b)
    0.0d0
  else
    term(a) + sum_double(term, next(a), next, b)
  end if;
end function sum_double;

define function pi_sum_(a :: <double-float>, b :: <double-float>) => result :: <double-float>;
  local
    method pi_term(x :: <double-float>) => result :: <double-float>;
      1.0d0 / (x * (x + 2.0d0));
    end,

    method pi_next(x :: <double-float>) => result :: <double-float>;
      x + 4.0d0;
    end;

  sum_double(pi_term, a, pi_next, b);
end function pi_sum_;

8.0 * pi_sum_(1.0, 1000.0);

define function integral(f :: <function>, a :: <double-float>, b :: <double-float>, dx :: <double-float>) => result :: <double-float>;
  local
    method add_dx(x :: <double-float>) => result :: <double-float>;
      x + dx;
    end;

  sum_double(f, a + (dx / 2.0d0), add_dx, b) * dx;
end function integral;

define function cube_double_(x :: <double-float>) => result :: <double-float>;
  x * x * x;
end function cube_double_;

integral(cube_double_, 0.0, 1.0, 0.01);
integral(cube_double_, 0.0, 1.0, 0.001);

/* Exercise 1.29 */
define function simpson(f :: <function>, a :: <double-float>, b :: <double-float>, n :: <integer>) => result :: <double-float>;
  let h = abs(b - a) / n;

  local
    method sum_iter(term :: <function>, start :: <integer>, next :: <function>, stop :: <integer>, acc :: <double-float>) => result :: <double-float>;
      if (start > stop)
        acc
      else
        sum_iter(term, next(start), next, stop, acc + term(a + start * h))
      end if;
    end;

  h * sum_iter(f, 1, inc_, n, 0.0d0);
end function simpson;

simpson(cube_double_, 0.0, 1.0, 100);

/* Exercise 1.30 */
define function sum_iter(term :: <function>, a :: <integer>, next :: <function>, b :: <integer>, acc :: <integer>) => result :: <integer>;
  if (a > b)
    acc
  else
    sum_iter(term, next(a), next, b, acc + term(a))
  end if;
end function sum_iter;

// 'sum_cubes_2' reimplements 'sum_cubes_' but uses 'sum_iter' in place of 'sum'
define function sum_cubes_2(a :: <integer>, b :: <integer>) => result :: <integer>;
  sum_iter(cube, a, inc_, b, 0);
end function sum_cubes_2;

sum_cubes_2(1, 10);

/* Exercise 1.31 */
// a.
define function product(term :: <function>, a :: <integer>, next :: <function>, b :: <integer>) => result :: <integer>;
  if (a > b)
    1
  else
    term(a) * product(term, next(a), next, b)
  end if;
end function product;

define function factorial(n :: <integer>) => result :: <integer>;
  product(identity_, 1, inc_, n);
end function factorial;

// b.
define function product_iter(term :: <function>, a :: <integer>, next :: <function>, b :: <integer>, acc :: <integer>) => result :: <integer>;
  if (a > b)
    acc
  else
    product_iter(term, next(a), next, b, acc * term(a))
  end if;
end function product_iter;

/* Exercise 1.32 */
// a.
define function accumulate_(combiner :: <function>, null-value :: <integer>, term :: <function>, a :: <integer>, next :: <function>, b :: <integer>) => result :: <integer>;
  if (a > b)
    null-value
  else
    combiner(term(a), accumulate_(combiner, null-value, term, next(a), next, b))
  end if;
end function accumulate_;

// sum:     accumulate_(\+, 0, identity_, a, inc_, b);
// product: accumulate_(\*, 1, identity_, a, inc_, b);

// b.
// NOTE: starting value of 'acc' is 'null-value'
define function accumulate_iter(combiner :: <function>, term :: <function>, a :: <integer>, next :: <function>, b :: <integer>, acc :: <integer>) => result :: <integer>;
  if (a > b)
    acc
  else
    accumulate_iter(combiner, term, next(a), next, b, combiner(acc, term(a)))
  end if;
end function accumulate_iter;

// sum:     accumulate_iter(\+, identity_, a, inc_, b, 0);
// product: accumulate_iter(\*, identity_, a, inc_, b, 1);

/* Exercise 1.33 */
define function filtered_accumulate(combiner :: <function>, null-value :: <integer>, term :: <function>, a :: <integer>, next :: <function>, b :: <integer>, pred :: <function>) => result :: <integer>;
  if (a > b)
    null-value
  else
    if (pred(a))
      combiner(term(a), filtered_accumulate(combiner, null-value, term, next(a), next, b, pred))
    else
      filtered_accumulate(combiner, null-value, term, next(a), next, b, pred)
    end if;
  end if;
end function filtered_accumulate;

// a.
filtered_accumulate(\+, 0, square, 1, inc_, 5, prime);  // 39

// b. Not sure how to implement this without modifying 'filtered_accumulate' to have 'pred'
//    accept two arguments
// 1.3.2 Formulating Abstractions with Higher-Order Procedures - Constructing Procedures Using Lambda
define function pi_sum_2(a :: <double-float>, b :: <double-float>) => result :: <double-float>;
  sum_double(method(x) 1.0d0 / (x * (x + 2.0d0)) end, a, method(x) x + 4.0d0 end, b);
end function pi_sum_2;

define function integral_1(f :: <function>, a :: <double-float>, b :: <double-float>, dx :: <double-float>) => result :: <double-float>;
  sum_double(f, a + (dx / 2.0d0), method(x) x + dx end, b) * dx;
end function integral_1;

define function plus4(x) x + 4; end;

let plus4_1 = method(x) x + 4; end;
plus4_1(4);

(method(x, y, z) x + y + square(z) end)(1, 2, 3);

// Using let
define function f_2(x :: <integer>, y :: <integer>) => result :: <integer>;
  local
    method f_helper(a :: <integer>, b :: <integer>) => result :: <integer>;
      (x * square(a)) + (y * b) + (a * b);
    end;
  f_helper(1 + x * y, 1 - y);
end function f_2;

define function f_3(x :: <integer>, y :: <integer>) => result :: <integer>;
  (method (a, b) (x * square(a)) + (y * b) + (a * b) end)(1 + x * y, 1 - y);
end function f_3;

define function f_4(x :: <integer>, y :: <integer>) => result :: <integer>;
  let a = 1 + x * y;
  let b = 1 - y;
  (x * square(a)) + (y * b) + (a * b);
end function f_4;

let x_1 = 5;

x_1 + block() let x_1 = 3; x_1 + (x_1 * 10); end block;

let x_2 = 2;

block()
  let y = x_2 + 2;
  block()
    let x_2 = 3;
    x_2 * y;
  end block;
end block;

define function f_5(x :: <integer>, y :: <integer>) => result :: <integer>;
  let a = 1 + (x * y);
  let b = 1 - y;
  (x + square(a)) + (y * b) + (a * b);
end function f_5;

/* Exercise 1.34 */
define function f_6(g :: <function>) => result :: <integer>;
  g(2);
end function f_6;

f_6(square);
f_6((method(z) z * (z + 1) end));
// 1.3.3 Formulating Abstractions with Higher-Order Procedures - Procedures as General Methods

// Half-interval method
define function close_enough(x :: <double-float>, y :: <double-float>) => result :: <boolean>;
  (abs_double(x - y) < 0.001d0);
end function close_enough;

define function positive_(x :: <double-float>) => result :: <boolean>;
  (x >= 0.0d0);
end function positive_;

define function negative_(x :: <double-float>) => result :: <boolean>;
  ~positive_(x);
end function negative_;

define function search(f :: <function>, neg_point :: <double-float>, pos_point :: <double-float>) => result :: <double-float>;
  let midpoint = average(neg_point, pos_point);
  if (close_enough(neg_point, pos_point))
    midpoint
  else
    let test_value = f(midpoint);
    if (positive_(test_value))
      search(f, neg_point, midpoint)
    elseif (negative_(test_value))
      search(f, midpoint, pos_point)
    else
      midpoint;
    end if;
  end if;
end function search;

define function half_interval_method(f :: <function>, a :: <double-float>, b :: <double-float>) => result :: <double-float>;
  let a_value = f(a); let b_value = f(b);

  if (negative_(a_value) & positive_(b_value))
    search(f, a, b)
  elseif (negative_(b_value) & positive_(a_value))
    search(f, b, a)
  else
    error(format-to-string("Values are not of opposite sign %= %=\n", a, b))
  end if;
end function half_interval_method;

half_interval_method(sin, 2.0, 4.0);

half_interval_method(method(x :: <double-float>) (x * x * x) - (2.0 * x) - 3.0 end, 1.0, 2.0);

// Fixed points
define variable *tolerance* = 0.00001d0;

define function fixed_point(f :: <function>, first_guess :: <double-float>) => result :: <double-float>;
  local
    method close_enough(v1 :: <double-float>, v2 :: <double-float>) => result :: <boolean>;
      abs_double(v1 - v2) < *tolerance*;
    end,

    method try_(guess :: <double-float>) => result :: <double-float>;
      let next = f(guess);
      if (close_enough(guess, next))
        next
      else
        try_(next)
      end if;
    end;

  try_(first_guess);
end function fixed_point;

fixed_point(cos, 1.0);

fixed_point(method(y :: <double-float>) sin(y) + cos(y) end, 1.0);

define function sqrt_4(x :: <double-float>) => result :: <double-float>;
  fixed_point(method(y :: <double-float>) x / y end, 1.0d0)
end function sqrt_4;

define function sqrt_5(x :: <double-float>) => result :: <double-float>;
  fixed_point(method(y :: <double-float>) average(y, x / y) end, 1.0d0);
end function sqrt_5;

/* Exercise 1.35 */
define function golden_ratio() => result :: <double-float>;
  fixed_point(method(x :: <double-float>) 1.0d0 + 1.0d0 / x end, 1.0d0)
end function golden_ratio;

/* Exercise 1.36 */
// Add the following line to function, 'fixed-point':
//  ... let next = f(guess);
//  format-out("[%=]\n", next);
//  ... if (close_enough(guess, next))

// 35 guesses before convergence
fixed_point(method(x :: <double-float>) log(1000.0) / log(x) end, 1.5);

// 11 guesses before convergence
fixed_point(average_damp(method(x :: <double-float>) log(1000.0) / log(x) end), 1.5);

/* Exercise 1.37 - unfinished */

/* Exercise 1.38 - unfinished */

/* Exercise 1.39 - unfinished */

// 1.3.4 Formulating Abstractions with Higher-Order Procedures - Procedures as Returned Values

define function average_damp(f :: <function>) => g :: <function>;
  method(x :: <double-float>) average(x, f(x)) end;
end function average_damp;

(average_damp(square_double))(10.0);

define function sqrt_6(x :: <double-float>) => result :: <double-float>;
  fixed_point(average_damp(method(y :: <double-float>) x / y end), 1.0d0);
end function sqrt_6;

define function cube_root(x :: <double-float>) => result :: <double-float>;
  fixed_point(average_damp(method(y :: <double-float>) x / square_double(y) end), 1.0d0);
end function cube_root;

// Newton's method
define variable *dx* = 0.00001d0;

define function deriv(g :: <function>) => g :: <function>;
  method(x :: <double-float>) (g(x + *dx*) - g(x)) / *dx* end;
end function deriv;

define function cube(x :: <double-float>) => result :: <double-float>;
  x * x * x;
end function cube;

define function newton_transform(g :: <function>) => g :: <function>;
  method(x :: <double-float>) x - (g(x) / (deriv(g)(x))) end;
end function newton_transform;

define function newtons_method(g :: <function>, guess :: <double-float>) => result :: <double-float>;
  fixed_point(newton_transform(g), guess);
end function newtons_method;

define function sqrt_7(x :: <double-float>) => result :: <double-float>;
  newtons_method(method(y :: <double-float>) square_double(y) - x end, 1.0d0);
end function sqrt_7;

// Fixed point of transformed function
define function fixed_point_of_transform(g :: <function>, transform :: <function>, guess :: <double-float>)  => result :: <double-float>;
  fixed_point(transform(g), guess);
end function fixed_point_of_transform;

define function sqrt_8(x :: <double-float>) => result :: <double-float>;
  fixed_point_of_transform(method(y :: <double-float>) x / y end, average_damp, 1.0d0);
end function sqrt_8;

define function sqrt_9(x :: <double-float>) => result :: <double-float>;
  fixed_point_of_transform(method(y :: <double-float>) square_double(y) - x end, newton_transform, 1.0d0);
end function sqrt_9;

/* Exercise 1.40 */
define function cubic(a :: <double-float>, b :: <double-float>, c :: <double-float>) => g :: <function>;
  method(x) cube(x) + a * x * x + b * x + c; end;
end function cubic;

newtons_method(cubic(5.0, 3.0, 2.5), 1.0); // -4.452...

/* Exercise 1.41 */
define function double(f :: <function>) => g :: <function>;
  method(x) f(f(x)); end;
end function double;

(double(inc_))(5);                         //  7
(double(double)(inc_))(5);                 //  9
(double(double(double))(inc_))(5);         // 21

/* Exercise 1.42 */
define function compose_(f :: <function>, g :: <function>) => h :: <function>;
  method(x) f(g(x)); end;
end function compose_;

(compose_(square, inc_))(6);               // 49

/* Exercise 1.43 */
define function repeated(f :: <function>, n :: <integer>) => g :: <function>;
  local
    method iterate_(arg :: <object>, i :: <integer>) => result :: <object>;
      if (i > n)
        arg
      else
        iterate_(f(arg), i + 1)
      end if;
    end;

  method(x) iterate_(x, 1); end;
end function repeated;

(repeated(square, 2))(5);                 // 625

/* Exercise 1.44 ('n-fold-smooth' not implemented) */
define function smooth(f :: <function>, dx :: <double-float>) => g :: <function>;
  method(x :: <double-float>) average(x, (f(x - dx) + f(x) + f(x + dx)) / 3.0d0) end;
end function smooth;

fixed_point(smooth(method(x :: <double-float>) log(1000.0) / log(x) end, 0.05), 1.5);

/* Exercise 1.45 - unfinished */

/* Exercise 1.46 ('sqrt' not implemented) */
define function iterative_improve(good_enough? :: <function>, improve :: <function>) => g :: <function>;
  local
    method iterate_(guess :: <object>) => result :: <object>;
      let next = improve(guess);
      if (good_enough?(guess, next))
        next
      else
        iterate_(next)
      end if;
    end;

  method(x) iterate_(x); end;
end function iterative_improve;

define function fixed_point_(f :: <function>, first_guess :: <double-float>) => result :: <double-float>;
  let tolerance = 0.00001d0;

  local
    method good_enough?(v1 :: <double-float>, v2 :: <double-float>) => result :: <boolean>;
      abs_double(v1 - v2) < tolerance;
    end;

  (iterative_improve(good_enough?, f))(first_guess);
end function fixed_point_;

fixed_point_(average_damp(method(x :: <double-float>) log(1000.0) / log(x) end), 1.5);
