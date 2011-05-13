#!/usr/bin/env rune
pragma.syntax("0.9")

# 1.1.1 The Elements of Programming - Expressions
486
137 + 349
1000 - 334
5 * 99
10 // 5
2.7 + 10
21 + 35 + 12 + 7
25 * 4 * 12
(3 * 5) + (10 - 6)
(3 * ((2 * 4) + (3 + 5))) + ((10 - 7) + 6)

# 1.1.2 The Elements of Programming - Naming and the Environment
var size := 2
size
5 * size
var pi := 3.14159
var radius := 10
pi * radius * radius
var circumference := 2 * pi * radius
circumference

# 1.1.3 The Elements of Programming - Evaluating Combinations
(2 + (4 * 6)) * (3 + 5 + 7)

# 1.1.4 The Elements of Programming - Compound Procedures
def square(x) { return (x * x) }
square(21)
square(2 + 5)
square(square(3))
def sum_of_squares(x, y) { return square(x) + square(y) }
sum_of_squares(3, 4)
def f(a) { return sum_of_squares(a + 1, a * 2) }
f(5)

# 1.1.5 The Elements of Programming - The Substitution Model for Procedure Application
f(5)
sum_of_squares(5 + 1, 5 * 2)
square(6) + square(10)
(6 * 6) + (10 * 10)
36 + 100
f(5)
sum_of_squares(5 + 1, 5 * 2)
square(5 + 1) + square(5 * 2)

((5 + 1) * (5 + 1)) + ((5 * 2) * (5 * 2))
(6 * 6) + (10 * 10)
36 + 100
136

# 1.1.6 The Elements of Programming - Conditional Expressions and Predicates
def abs(x) {
   if (x > 0) {
      return x
   } else if (x == 0) {
      return 0
   } else {
      return -x
   }
}

def abs(x) {
   if (x < 0) {
      return -x
   } else {
      return x
   }
}

# alternative translation in a more functional style
def abs(x) {
   return if (x < 0) { -x } else { x }
}

var x := 6
(x > 5) && (x < 10)
def ge(x, y) {
   return (x > y) || (x == y)
}
def ge(x, y) {
   return !(x < y)
}

# Exercise 1.1 #
10
5 + 3 + 4
9 - 1
6 // 2
(2 * 4) + (4 - 6)
var a := 3
var b := a + 1
a + b + (a * b)
(a == b)
if ((b > a) && (b < (a * b))) { b } else { a }
if (a == 4) { 6 } else if (b == 4) { (6 + 7 + a) } else { 25 }
2 + (if (b > a) { b } else { a })
(if (a > b) { a } else if (a < b) { b } else { -1 }) * (a + 1)

# Exercise 1.2 #
((5 + 4 + (2 - (3 - (6 + (4 / 5))))) / (3 * (6 - 2) * (2 - 7)))

# Exercise 1.4 #
def a_plus_abs_b(a, b) {
   if (b > 0) {
      return a + b
   } else {
      return a - b
   }
}

# Exercise 1.5 #
def p() { return p() }
def test(x, y) {
   if (x == 0) {
      return 0
   } else {
      return y
   }
}
# commented out as this is in infinite loop
# test(0, p())

# 1.1.7 The Elements of Programming - Example: Square Roots by Newton's Method

# same as above
# def square(x) { return x * x }

def good_enough(guess, x) {
   return abs(square(guess) - x) < 0.001
}

def average(x, y) {
   return (x + y) / 2
}

def improve(guess, x) {
   return average(guess, x / guess)
}

def sqrt_iter(guess, x) {
   if (good_enough(guess, x)) {
      return guess
   } else {
      return sqrt_iter(improve(guess, x), x)
   }
}

def sqrt(x) {
   return sqrt_iter(1, x)
}

sqrt(9)
sqrt(100 + 37)
sqrt(sqrt(2)+sqrt(3))
square(sqrt(1000))

# Exercise 1.6 #
def new_if(predicate, then_clause, else_clause) {
   if (predicate) {
      return then_clause
   } else {
      return else_clause
   }
}
new_if((2==3), 0, 5)
new_if((1==1), 0, 5)
def sqrt_iter(guess, x) {
   return new_if(
      good_enough(guess, x),
      guess,
      sqrt_iter(improve(guess, x), x))
}

# 1.1.8 The Elements of Programming - Procedures as Black-Box Abstractions

# same as above
# def square(x) { return x * x }

def double(x) { return x + x }

def square_real(x) { return double(x.asFloat64().log()).exp() }

def good_enough(guess, x) {
   return abs(square(guess) - x) < 0.001
}

def improve(guess, x) {
   return average(guess, x / guess)
}

def sqrt_iter(guess, x) {
   if (good_enough(guess, x)) {
      return guess
   } else {
      return sqrt_iter(improve(guess, x), x)
   }
}

def sqrt(x) {
   return sqrt_iter(1.0, x)
}

square(5)

# Block-structured
def sqrt(x) {
   def good_enough(guess, x) {
      return abs(square(guess) - x) < 0.001
   }

   def improve(guess, x) {
      return average(guess, x / guess)
   }

   def sqrt_iter(guess, x) {
      if (good_enough(guess, x)) {
         return guess
      } else {
         return sqrt_iter(improve(guess, x), x)
      }
   }

   return sqrt_iter(1.0, x)
}

# Taking advantage of lexical scoping
def sqrt(x) {
   def good_enough(guess) {
      return abs(square(guess) - x) < 0.001
   }

   def improve(guess) {
      return average(guess, x / guess)
   }

   def sqrt_iter(guess) {
      if (good_enough(guess)) {
         return guess
      } else {
         return sqrt_iter(improve(guess))
      }
   }

   return sqrt_iter(1.0)
}


# 1.2.1 Procedures and the Processes They Generate - Linear Recursion and Iteration

# Recursive
def factorial(n) {
   if (n == 1) {
      return 1
   } else {
      return n * factorial(n - 1)
   }
}

# Iterative
def fact_iter(product, counter, max_count) {
   if (counter > max_count) {
      return product
   } else {
      return fact_iter(counter * product, counter + 1, max_count)
   }
}

def factorial(n) {
   return fact_iter(1, 1, n)
}

# Iterative, block-structured (from footnote)
def factorial(n) {
   def iter(product, counter) {
      if (counter > n) {
         return product
      } else {
         return iter(counter * product, counter + 1)
      }
   }
   return iter(1, 1)
}

# Exercise 1.9 #
def inc(a) { return a + 1 }
def dec(a) { return a - 1 }
def plus(a, b) {
   if (a == 0) {
      return b
   } else {
      return inc(dec(a) + b)
   }
}
def plus(a, b) {
   if (a == 0) {
      return b
   } else {
      return dec(a) + inc(b)
   }
}

# Exercise 1.10 #
def a(x, y) {
   if (y == 0) {
      return 0
   } else if (x == 0) {
      return 2 * y
   } else if (y == 1) {
      return 2
   } else {
      return a(x - 1, a(x, y - 1))
   }
}
a(1, 10)
a(2, 4)
a(3, 3)
def f(n) { return a(0, n) }
def g(n) { return a(1, n) }
def h(n) { return a(2, n) }
def k(n) { return 5 * n * n }

# 1.2.2 Procedures and the Processes They Generate - Tree Recursion

# Recursive
def fib(n) {
   if (n == 0) {
      return 0
   } else if (n == 1) {
      return 1
   } else {
      return fib(n - 1) + fib(n - 2)
   }
}

# Iterative
def fib_iter(a, b, count) {
   if (count == 0) {
      return b
   } else {
      return fib_iter(a + b, a, count - 1)
   }
}
def fib(n) {
   return fib_iter(1, 0, n)
}

# Counting change
def first_denomination(x) {
   if (x == 1) {
      return 1
   } else if (x == 2) {
      return 5
   } else if (x == 3) {
      return 10
   } else if (x == 4) {
      return 25
   } else if (x == 5) {
      return 50
   }
}

def cc(amount, kinds_of_coins) {
   if (amount == 0) {
      return 1
   } else if (amount < 0) {
      return 0
   } else if (kinds_of_coins == 0) {
      return 0
   } else {
      return (cc(amount, kinds_of_coins - 1) +
              cc(amount - first_denomination(kinds_of_coins), kinds_of_coins))
   }
}

def count_change(amount) {
   return cc(amount, 5)
}

count_change(100)

# Exercise 1.15 #
def cube(x) { return x * x * x }
def p(x) { return (3.0 * x) - (4.0 * cube(x)) }
def sine(angle) {
   if (!abs(angle) > 0.1) {
      return angle
   } else {
      return p(sine(angle / 3.0))
   }
}

# 1.2.4 Procedures and the Processes They Generate - Exponentiation

# Linear recursion
def expt(b, n) {
   if (n == 0) {
      return 1
   } else {
      return b * expt(b, (n - 1))
   }
}

# Linear iteration
def expt_iter(b, counter, product) {
   if (counter == 0) {
      return product
   } else {
      return expt_iter(b, counter - 1, b * product)
   }
}
def expt(b, n) {
   return expt_iter(b, n, 1)
}

# Logarithmic iteration
def even(n) { return ((n % 2) == 0) }

def fast_expt(b, n) {
   if (n == 0) {
      return 1
   } else {
      if (even(n)) {
         return square(fast_expt(b, n // 2))
      } else {
         return b * fast_expt(b, n - 1)
      }
   }
}

# Exercise 1.17 #
def multiply(a, b) {
   if (b == 0) {
      return 0
   } else {
      return a + (a * (b - 1))
   }
}

# Exercise 1.19 #
# exercise left to reader to solve for p' and q'
# def fib_iter_x(a, b, p, q, count) {
#    if (count == 0) {
#       return b
#    } else {
#       if (even(count)) {
#          return fib_iter(a, b, p_x, q_x, count // 2)
#       } else {
#          return fib_iter((b * q) + (a * q) + (a * p), (b * p) + (a * q), p, q, count - 1)
#       }
#    }
# }
# def fib(n) {
#    return fib_iter(1, 0, 0, 1, n)
# }


# 1.2.5 Procedures and the Processes They Generate - Greatest Common Divisors
def gcd(a, b) {
   if (b == 0) {
      return a
   } else {
      return gcd(b, a % b)
   }
}

# 1.2.6 Procedures and the Processes They Generate - Example: Testing for Primality

# prime
def divides(a, b) { return ((b % a) == 0) }

def find_divisor(n, test_divisor) {
   if (square(test_divisor) > n) {
      return n
   } else if (divides(test_divisor, n)) {
      return test_divisor
   } else {
      return find_divisor(n, test_divisor + 1)
   }
}

def smallest_divisor(n) { return find_divisor(n, 2) }

def prime(n) { return (n == smallest_divisor(n)) }

# fast_prime
def expmod(nbase, nexp, m) {
   if (nexp == 0) {
      return 1
   } else {
      if (even(nexp)) {
         return square(expmod(nbase, nexp // 2, m)) % m
      } else {
         return (nbase * (expmod(nbase, (nexp - 1), m))) % m
      }
   }
}

def fermat_test(n) {
   def try_it(a) { return (expmod(a, n, n) == a) }
   return try_it(1 + entropy.nextInt(n-1))
}

def fast_prime(n, ntimes) {
   if (ntimes == 0) {
      return true
   } else {
      if (fermat_test(n)) {
         return fast_prime(n, ntimes - 1)
      } else {
         return false
      }
   }
}

# Exercise 1.22 #
def report_prime(elapsed_time) {
   print(" *** ")
   println(elapsed_time)
}
def start_prime_test(n, start_time) {
   if (prime(n)) {
      report_prime(timer.now() - start_time)
   }
}
def timed_prime_test(n) {
   println(n)
   start_prime_test(n, timer.now())
}

# Exercise 1.25 #
def expmod(nbase, nexp, m) {
   return fast_expt(nbase, nexp) % m
}

# Exercise 1.26 #
def expmod(nbase, nexp, m) {
   if (nexp == 0) {
      return 1
   } else {
      if (even(nexp)) {
         return (expmod(nbase, (nexp // 2), m) * expmod(nbase, (nexp // 2), m)) % m
      } else {
         return (nbase * expmod(nbase, nexp - 1, m)) % m
      }
   }
}

# 1.3 Formulating Abstractions with Higher-Order Procedures
def cube(x) { return x * x * x }

# 1.3.1 Formulating Abstractions with Higher-Order Procedures - Procedures as Arguments
def sum_integers(a, b) {
   if (a > b) {
      return 0
   } else {
      return a + sum_integers(a + 1, b)
   }
}

def sum_cubes(a, b) {
   if (a > b) {
      return 0
   } else {
      return cube(a) + sum_cubes(a + 1, b)
   }
}

def pi_sum(a, b) {
   if (a > b) {
      return 0.0
   } else {
      return (1.0 / (a * (a + 2.0))) + pi_sum(a + 4.0, b)
   }
}

def sum(term, a, next, b) {
   if (a > b) {
      return 0
   } else {
      return term(a) + sum(term, next(a), next, b)
   }
}

# Using sum
# same as above
# def inc(n) { return n + 1 }

def sum_cubes(a, b) {
   return sum(cube, a, inc, b)
}

sum_cubes(1, 10)

def identity(x) { return x }

def sum_integers(a, b) {
   return sum(identity, a, inc, b)
}

sum_integers(1, 10)

def pi_sum(a, b) {
   def pi_term(x) { return 1.0 / (x * (x + 2.0)) }
   def pi_next(x) { return x + 4.0 }
   return sum(pi_term, a, pi_next, b)
}

8.0 * pi_sum(1, 1000)

def integral(f, a, b, dx) {
   def add_dx(x) { return x + dx }
   return sum(f, a + (dx / 2.0), add_dx, b) * dx
}

# same as above
#def cube(x) { return x * x * x }

integral(cube, 0.0, 1.0, 0.01)
# exceeds maximum recursion depth on Java implementation.  Should work in CL implementation.
# integral(cube, 0.0, 1.0, 0.001)

# Exercise 1.32 #
# exercise left to reader to define appropriate functions
# accumulate(combiner, null_value, term, a, next, b)

# 1.3.2 Formulating Abstractions with Higher-Order Procedures - Constructing Procedures Using Lambda
def pi_sum(a, b) {
   return sum(def _(x) { return 1.0 / (x * (x + 2.0)) }, a, def _(x) { return x + 4.0 }, b)
}

def integral(f, a, b, dx) {
   return sum(f, a + (dx / 2.0), def _(x) { return x + dx }, b) * dx
}

def plus4(x) { return x + 4 }

var plus4 := def _(x) { return x + 4 }

(def _(x, y, z) { return x + y + square(z) }) (1, 2, 3)

# alternative syntax for lambda functions
var plus4 := fn x { x + 4 }
(fn x, y, z { x + y + square(z) }) (1, 2, 3)


# Using let
def f(x, y) {
   def f_helper(a, b) {
      return (x * square(a)) + (y * b) + (a * b)
   }
   return f_helper(1 + (x * y), 1 - y)
}

def f(x, y) {
   return (def _(a, b) { return (x * square(a)) + (y * b) + (a * b) }) (1 + (x * y), 1 - y)
}

def f(x, y) {
   var a := 1 + (x * y)
   var b := 1 - y
   return (x * square(a)) + (y * b) + (a * b)
}

var x := 5
{
   var x := 3
   x + (x * 10)
} + x

var x := 2
{
   var y := x + 2
   var x := 3
   x * y
}

def f(x, y) {
   var a := 1 + (x * y)
   var b := 1 - y
   return (x + square(a)) + (y * b) + (a * b)
}

# Exercise 1.34 #
def f(g) { return g(2) }
f(square)
f(def _(z) { return z * (z + 1) })

# 1.3.3 Formulating Abstractions with Higher-Order Procedures - Procedures as General Methods

# Half-interval method
def close_enough(x, y) {
   return (abs(x - y) < 0.001)
}

def positive(x) { return (x >= 0.0) }
def negative(x) { return !positive(x) }

def search(f, neg_point, pos_point) {
   var midpoint := average(neg_point, pos_point)
   if (close_enough(neg_point, pos_point)) {
      return midpoint
   } else {
      var test_value := f(midpoint)
      if (positive(test_value)) {
         return search(f, neg_point, midpoint)
      } else if (negative(test_value)) {
         return search(f, midpoint, pos_point)
      } else {
         return midpoint
      }
   }
}

def half_interval_method(f, a, b) {
   var a_value := f(a)
   var b_value := f(b)
   if (negative(a_value) && positive(b_value)) {
      return search(f, a, b)
   } else if (negative(b_value) && positive(a_value)) {
      return search(f, b, a)
   } else {
      throw(`Values are not of opposite sign $a $b`)
   }
}

half_interval_method(def _(x) { return x.sin() }, 2.0, 4.0)

half_interval_method(def _(x) { return (x * x * x) - (2.0 * x) - 3.0 }, 1.0, 2.0)

# Fixed points
var tolerance := 0.00001

def fixed_point(f, first_guess) {
   def close_enough(v1, v2) {
      return abs(v1 - v2) < tolerance
   }
   def tryit(guess) {
      var next := f(guess)
      if (close_enough(guess, next)) {
         return next
      } else {
         return tryit(next)
      }
   }
   return tryit(first_guess)
}

fixed_point(def _(x) { return x.cos() }, 1.0)

fixed_point(def _(y) { return y.sin() + y.cos() }, 1.0)

# note: this function does not converge
def sqrt(x) {
   return fixed_point(def _(y) { return x / y }, 1.0)
}

def sqrt(x) {
   return fixed_point(def _(y) { return average(y, x / y) }, 1.0)
}

# Exercise 1.37 #
# exercise left to reader to define cont_frac
# cont_frac(def _(i) { return 1.0 }, def _(i) { return 1.0 }, k)


# 1.3.4 Formulating Abstractions with Higher-Order Procedures - Procedures as Returned Values
def average_damp(f) {
   return (def _(x) {return average(x, f(x)) })
}

(average_damp(square)) (10.0)

def sqrt(x) {
   return fixed_point(average_damp(def _(y) { return x / y }), 1.0)
}

def cube_root(x) {
   return fixed_point(average_damp(def _(y) { return x / square(y) }), 1.0)
}

cube_root(8)

# Newton's method
var dx := 0.00001
def deriv(g) {
   return (def _(x) { return (g(x + dx) - g(x)) / dx })
}

# same as above
#def cube(x) { return x * x * x }

deriv(cube) (5.0)

def newton_transform(g) {
   return (def _(x) { return x - (g(x) / (deriv(g) (x))) })
}

def newtons_method(g, guess) {
   return fixed_point(newton_transform(g), guess)
}

def sqrt(x) {
   return newtons_method(def _(y) { return square(y) - x }, 1.0)
}

# Fixed point of transformed function
def fixed_point_of_transform(g, transform, guess) {
   return fixed_point(transform(g), guess)
}

def sqrt(x) {
   return fixed_point_of_transform(def _(y) { return x / y }, average_damp, 1.0)
}

def sqrt(x) {
   return fixed_point_of_transform(def _(y) { return square(y) - x }, newton_transform, 1.0)
}

# Exercise 1.40 #
# exercise left to reader to define cubic
# newtons_method(cubic(a, b, c), 1.0)

# Exercise 1.41 #
# exercise left to reader to define double
# (double(double(double)))(inc) (5)

# Exercise 1.42 #
# exercise left to reader to define compose
# (compose(square, inc)) (6)

# Exercise 1.43 #
# exercise left to reader to define repeated
# (repeated(square, 2)) (5)
