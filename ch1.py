#!/usr/local/bin/python
# -*- coding: UTF-8 -*-
import math
import random
import time

# 1.1.1 The Elements of Programming - Expressions
print 486
print 137 + 349
print 1000 - 334
print 5 * 99
print 10 / 5
print 2.7 + 10
print 21 + 35 + 12 + 7
print 25 * 4 * 12
print (3 * 5) + (10 - 6)
print (3 * ((2 * 4) + (3 + 5))) + ((10 - 7) + 6)

# 1.1.2 The Elements of Programming - Naming and the Environment
size = 2
print size
print 5 * size
pi = 3.14159
radius = 10
print pi * radius * radius
circumference = 2 * pi * radius
print circumference

# 1.1.3 The Elements of Programming - Evaluating Combinations
print (2 + (4 * 6)) * (3 + 5 + 7)

# 1.1.4 The Elements of Programming - Compound Procedures
def square(x): return (x * x)
print square(21)
print square(2 + 5)
print square(square(3))
def sum_of_squares(x, y): return square(x) + square(y)
print sum_of_squares(3, 4)
def f(a): return sum_of_squares(a + 1, a * 2)
print f(5)

# 1.1.5 The Elements of Programming - The Substitution Model for Procedure Application
print f(5)
print sum_of_squares(5 + 1, 5 * 2)
print square(6) + square(10)
print (6 * 6) + (10 * 10)
print 36 + 100
print f(5)
print sum_of_squares(5 + 1, 5 * 2)
print square(5 + 1) + square(5 * 2)

print ((5 + 1) * (5 + 1)) + ((5 * 2) * (5 * 2))
print (6 * 6) + (10 * 10)
print 36 + 100
print 136

# 1.1.6 The Elements of Programming - Conditional Expressions and Predicates
def abs(x):
   if (x > 0):
      return x
   elif (x == 0):
      return 0
   else:
      return -x
def abs(x):
   if (x < 0):
      return -x
   else:
      return x
x = 6
print (x > 5) and (x < 10)
def ge(x, y):
   return (x > y) or (x == y)
def ge(x, y):
   return not(x < y)

# Exercise 1.1 #
print 10
print 5 + 3 + 4
print 9 - 1
print 6 / 2
print (2 * 4) + (4 - 6)
a = 3
b = a + 1
print a + b + (a * b)
print (a == b)
print b if ((b > a) and (b < (a *b))) else a
print 6 if (a == 4) else (6 + 7 + a) if (b == 4) else 25
print 2 + (b if (b > a) else a)
print  (a if (a > b) else b if (a < b) else -1) * (a + 1)

# Exercise 1.2 #
print ((5.0 + 4.0 + (2.0 - (3.0 - (6.0 + (4.0 / 5.0))))) /
         (3.0 * (6.0 - 2.0) * (2.0 - 7.0)))

# Exercise 1.4 #
def a_plus_abs_b(a, b):
   if (b > 0):
      return a + b
   else:
      return a - b

# Exercise 1.5 #
def p(): return p()
def test(x, y):
   if (x == 0):
      return 0
   else:
      return y
# commented out as this is in infinite loop
# test(0, p())

# 1.1.7 The Elements of Programming - Example: Square Roots by Newton's Method
def square(x): return x * x

def good_enough(guess, x):
   return abs(square(guess) - x) < 0.001

def average(x, y):
   return (x + y) / 2.0

def improve(guess, x):
   return average(guess, float(x) / guess)

def sqrt_iter(guess, x):
   if (good_enough(guess, x)):
      return guess
   else:
      return sqrt_iter(improve(guess, x), x)

def sqrt(x):
   return sqrt_iter(1.0, float(x))

print sqrt(9)
print sqrt(100 + 37)
print sqrt(sqrt(2)+sqrt(3))
print square(sqrt(1000))

# Exercise 1.6 #
def new_if(predicate, then_clause, else_clause):
   if (predicate):
      return then_clause
   else:
      return else_clause
print new_if((2==3), 0, 5)
print new_if((1==1), 0, 5)
def sqrt_iter(guess, x):
   return new_if(
      good_enough(guess, x),
      guess,
      sqrt_iter(improve(guess, x), x))

# 1.1.8 The Elements of Programming - Procedures as Black-Box Abstractions
def square(x): return x * x

def double(x): return x + x

def square_real(x): return math.exp(double(math.log(x)))

def good_enough(guess, x):
   return abs(square(guess) - x) < 0.001

def improve(guess, x):
   return average(guess, float(x) / guess)

def sqrt_iter(guess, x):
   if (good_enough(guess, x)):
      return guess
   else:
      return sqrt_iter(improve(guess, x), x)

def sqrt(x):
   return sqrt_iter(1.0, x)

print square(5)

# Block-structured
def sqrt(x):
   def good_enough(guess, x):
      return abs(square(guess) - x) < 0.001

   def improve(guess, x):
      return average(guess, float(x) / guess)

   def sqrt_iter(guess, x):
      if (good_enough(guess, x)):
         return guess
      else:
         return sqrt_iter(improve(guess, x), x)

   return sqrt_iter(1.0, x)

# Taking advantage of lexical scoping
def sqrt(x):
   def good_enough(guess):
      return abs(square(guess) - x) < 0.001

   def improve(guess):
      return average(guess, float(x) / guess)

   def sqrt_iter(guess):
      if (good_enough(guess)):
         return guess
      else:
         return sqrt_iter(improve(guess))

   return sqrt_iter(1.0)

# 1.2.1 Procedures and the Processes They Generate - Linear Recursion and Iteration

# Recursive
def factorial(n):
   if (n == 1):
      return 1
   else:
      return n * factorial(n - 1)

# Iterative
def fact_iter(product, counter, max_count):
   if (counter > max_count):
      return product
   else:
      return fact_iter(counter * product, counter + 1, max_count)

def factorial(n):
   return fact_iter(1, 1, n)

# Iterative, block-structured (from footnote)
def factorial(n):
   def iter(product, counter):
      if (counter > n):
         return product
      else:
         return iter(counter * product, counter + 1)
   return iter(1, 1)

# Exercise 1.9 #
def inc(a): return a + 1
def dec(a): return a - 1
def plus(a, b):
   if (a == 0):
      return b
   else:
      return inc(dec(a) + b)
def plus(a, b):
   if (a == 0):
      return b
   else:
      return dec(a) + inc(b)

# Exercise 1.10 #
def a(x, y):
   if (y == 0):
      return 0
   elif (x == 0):
      return 2 * y
   elif (y == 1):
      return 2
   else:
      return a(x - 1, a(x, y - 1))
print a(1, 10)
print a(2, 4)
print a(3, 3)
def f(n): return a(0, n)
def g(n): return a(1, n)
def h(n): return a(2, n)
def k(n): return 5 * n * n

# 1.2.2 Procedures and the Processes They Generate - Tree Recursion

# Recursive
def fib(n):
   if (n == 0):
      return 0
   elif (n == 1):
      return 1
   else:
      return fib(n - 1) + fib(n - 2)

# Iterative
def fib_iter(a, b, count):
   if (count == 0):
      return b
   else:
      return fib_iter(a + b, a, count - 1)
def fib(n):
   return fib_iter(1, 0, n)

# Counting change
def first_denomination(x):
   if (x == 1):
      return 1
   elif (x == 2):
      return 5
   elif (x == 3):
      return 10
   elif (x == 4):
      return 25
   elif (x == 5):
      return 50

def cc(amount, kinds_of_coins):
   if (amount == 0):
      return 1
   elif (amount < 0):
      return 0
   elif (kinds_of_coins == 0):
      return 0
   else:
      return (cc(amount, kinds_of_coins - 1) +
                 cc(amount - first_denomination(kinds_of_coins), kinds_of_coins))

def count_change(amount):
   return cc(amount, 5)

print count_change(100)

# Exercise 1.15 #
def cube(x): return x * x * x
def p(x): return (3.0 * x) - (4.0 * cube(x))
def sine(angle):
   if not(abs(angle) > 0.1):
      return angle
   else:
      return p(sine(angle / 3.0))

# 1.2.4 Procedures and the Processes They Generate - Exponentiation

# Linear recursion
def expt(b, n):
   if (n == 0):
      return 1
   else:
      return b * expt(b, (n - 1))

# Linear iteration
def expt_iter(b, counter, product):
   if (counter == 0):
      return product
   else:
      return expt_iter(b, counter - 1, b * product)
def expt(b, n):
   return expt_iter(b, n, 1)

# Logarithmic iteration
def even(n): return ((n % 2) == 0)

def fast_expt(b, n):
   if (n == 0):
      return 1
   else:
      if (even(n)):
         return square(fast_expt(b, n / 2))
      else:
         return b * fast_expt(b, n - 1)

# Exercise 1.17 #
def multiply(a, b):
   if (b == 0):
      return 0
   else:
      return a + (a * (b - 1))

# Exercise 1.19 #
# exercise left to reader to solve for p' and q'
#def fib_iter(a, b, p, q, count):
#   if (count == 0):
#      return b
#   else:
#      if (even(count)):
#         return fib_iter(a, b, p', q', count / 2)
#      else:
#         return fib_iter((b * q) + (a * q) + (a * p), (b * p) + (a * q), p, q, count - 1)
#def fib(n):
#   return fib_iter(1, 0, 0, 1, n)


# 1.2.5 Procedures and the Processes They Generate - Greatest Common Divisors
def gcd(a, b):
   if (b == 0):
      return a
   else:
      return gcd(b, a % b)

# 1.2.6 Procedures and the Processes They Generate - Example: Testing for Primality

# prime
def divides(a, b): return ((b % a) == 0)

def find_divisor(n, test_divisor):
   if (square(test_divisor) > n):
      return n
   elif (divides(test_divisor, n)):
      return test_divisor
   else:
      return find_divisor(n, test_divisor + 1)

def smallest_divisor(n): return find_divisor(n, 2)

def prime(n): return (n == smallest_divisor(n))

# fast_prime
def expmod(nbase, nexp, m):
   if (nexp == 0):
      return 1
   else:
      if (even(nexp)):
         return square(expmod(nbase, nexp / 2, m)) % m
      else:
         return (nbase * (expmod(nbase, (nexp - 1), m))) % m

def fermat_test(n):
   def try_it(a): return (expmod(a, n, n) == a)
   return try_it(random.randint(0, n-1))

def fast_prime(n, ntimes):
   if (ntimes == 0):
      return True
   else:
      if (fermat_test(n)):
         return fast_prime(n, ntimes - 1)
      else:
         return False

# Exercise 1.22 #
def report_prime(elapsed_time):
   print " *** " + str(elapsed_time)
def start_prime_test(n, start_time):
   if (prime(n)):
      report_prime(time.time() - start_time)
def timed_prime_test(n):
   print "\n" + str(n)
   start_prime_test(n, time.time())

# Exercise 1.25 #
def expmod(nbase, nexp, m):
   return fast_expt(nbase, nexp) % m

# Exercise 1.26 #
def expmod(nbase, nexp, m):
   if (nexp == 0):
      return 1
   else:
      if (even(nexp)):
         return (expmod(nbase, (nexp / 2), m) * expmod(nbase, (nexp / 2), m)) % m
      else:
         return (nbase * expmod(nbase, nexp - 1, m)) % m

# 1.3 Formulating Abstractions with Higher-Order Procedures
def cube(x): return x * x * x

# 1.3.1 Formulating Abstractions with Higher-Order Procedures - Procedures as Arguments
def sum_integers(a, b):
   if (a > b):
      return 0
   else:
      return a + sum_integers(a + 1, b)

def sum_cubes(a, b):
   if (a > b):
      return 0
   else:
      return cube(a) + sum_cubes(a + 1, b)

def pi_sum(a, b):
   if (a > b):
      return 0.0
   else:
      return (1.0 / (a * (a + 2.0))) + pi_sum(a + 4.0, b)

def sum(term, a, next, b):
   if (a > b):
      return 0
   else:
      return term(a) + sum(term, next(a), next, b)

# Using sum
def inc(n): return n + 1

def sum_cubes(a, b):
   return sum(cube, a, inc, b)

print sum_cubes(1, 10)

def identity(x): return x

def sum_integers(a, b):
   return sum(identity, a, inc, b)

print sum_integers(1, 10)

def pi_sum(a, b):
   def pi_term(x): return 1.0 / (x * (x + 2.0))
   def pi_next(x): return x + 4.0
   return sum(pi_term, a, pi_next, b)

print 8.0 * pi_sum(1, 1000)

def integral(f, a, b, dx):
   def add_dx(x): return x + dx
   return sum(f, a + (dx / 2.0), add_dx, b) * dx

def cube(x): return x * x * x

print integral(cube, 0.0, 1.0, 0.01)
# exceeds maximum recursion depth
# print integral(cube, 0.0, 1.0, 0.001)

# Exercise 1.32 #
# exercise left to reader to define appropriate functions
# print accumulate(combiner, null_value, term, a, next, b)

# 1.3.2 Formulating Abstractions with Higher-Order Procedures - Constructing Procedures Using Lambda
def pi_sum(a, b):
   return sum(lambda x: 1.0 / (x * (x + 2.0)), a, lambda x: x + 4.0, b)

def integral(f, a, b, dx):
   return sum(f, a + (dx / 2.0), lambda x: x + dx, b) * dx

def plus4(x): return x + 4

plus4 = lambda x: x + 4

print (lambda x, y, z: x + y + square(z)) (1, 2, 3)

# Using let
def f(x, y):
   def f_helper(a, b):
      return (x * square(a)) + (y * b) + (a * b)
   return f_helper(1 + (x * y), 1 - y)

def f(x, y):
   return (lambda a, b: (x * square(a)) + (y * b) + (a * b)) (1 + (x * y), 1 - y)

def f(x, y):
   a = 1 + (x * y)
   b = 1 - y
   return (x * square(a)) + (y * b) + (a * b)

# python does not have let binding and lambdas are limited to an expression
# so we'll use default parameters to emulate (courtesy of Randy Hudson)
x = 5
print (lambda x=3: x + x*10)() + x

x = 2
print (lambda x=3,y=x+2: x*y)()

def f(x, y):
   a = 1 + (x * y)
   b = 1 - y
   return (x + square(a)) + (y * b) + (a * b)

# Exercise 1.34 #
def f(g): return g(2)
print f(square)
print f(lambda z: z * (z + 1))

# 1.3.3 Formulating Abstractions with Higher-Order Procedures - Procedures as General Methods

# Half-interval method
def close_enough(x, y):
   return (abs(x - y) < 0.001)

def positive(x): return (x >= 0.0)
def negative(x): return not(positive(x))

def search(f, neg_point, pos_point):
   midpoint = average(neg_point, pos_point)
   if (close_enough(neg_point, pos_point)):
      return midpoint
   else:
      test_value = f(midpoint)
      if (positive(test_value)):
         return search(f, neg_point, midpoint)
      elif (negative(test_value)):
         return search(f, midpoint, pos_point)
      else:
         return midpoint

def half_interval_method(f, a, b):
   a_value = f(a)
   b_value = f(b)
   if (negative(a_value) and positive(b_value)):
      return search(f, a, b)
   elif (negative(b_value) and positive(a_value)):
      return search(f, b, a)
   else:
      raise Exception("Values are not of opposite sign " + str(a) + " " + str(b))

print half_interval_method(math.sin, 2.0, 4.0)

print half_interval_method(lambda x: (x * x * x) - (2.0 * x) - 3.0, 1.0, 2.0)

# Fixed points
tolerance = 0.00001

def fixed_point(f, first_guess):
   def close_enough(v1, v2):
      return abs(v1 - v2) < tolerance
   def tryit(guess):
      next = f(guess)
      if (close_enough(guess, next)):
         return next
      else:
         return tryit(next)
   return tryit(first_guess)

print fixed_point(math.cos, 1.0)

print fixed_point(lambda y: math.sin(y) + math.cos(y), 1.0)

# note: this function does not converge
def sqrt(x):
   return fixed_point(lambda y: float(x) / y, 1.0)

def sqrt(x):
   return fixed_point(lambda y: average(y, float(x) / y), 1.0)

# Exercise 1.37 #
# exercise left to reader to define cont_frac
# cont_frac(lambda i: return 1.0, lambda i: return 1.0, k)

# 1.3.4 Formulating Abstractions with Higher-Order Procedures - Procedures as Returned Values
def average_damp(f):
   return (lambda x: average(float(x), f(x)))

print (average_damp(square)) (10.0)

def sqrt(x):
   return fixed_point(average_damp(lambda y: float(x) / y), 1.0)

def cube_root(x):
   return fixed_point(average_damp(lambda y: float(x) / square(y)), 1.0)

print cube_root(8)

# Newton's method
dx = 0.00001
def deriv(g):
   return (lambda x: float(g(x + dx) - g(x)) / dx)

def cube(x): return x * x * x

print deriv(cube) (5.0)

def newton_transform(g):
   return (lambda x: x - (float(g(x)) / (deriv(g) (x))))

def newtons_method(g, guess):
   return fixed_point(newton_transform(g), guess)

def sqrt(x):
   return newtons_method(lambda y: square(y) - x, 1.0)

# Fixed point of transformed function
def fixed_point_of_transform(g, transform, guess):
   return fixed_point(transform(g), guess)

def sqrt(x):
   return fixed_point_of_transform(lambda y: x / y, average_damp, 1.0)

def sqrt(x):
   return fixed_point_of_transform(lambda y: square(y) - x, newton_transform, 1.0)

# Exercise 1.40 #
# exercise left to reader to define cubic
# print newtons_method(cubic(a, b, c), 1.0)

# Exercise 1.41 #
# exercise left to reader to define double
# print (double(double(double)))(inc) (5)

# Exercise 1.42 #
# exercise left to reader to define compose
# print (compose(square, inc)) (6)

# Exercise 1.43 #
# exercise left to reader to define repeated
# print (repeated(square, 2)) (5)
