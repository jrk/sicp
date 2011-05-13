# 1.1.1 The Elements of Programming - Expressions
486
137 + 349
1000 - 334
5 * 99
10 / 5
2.7 + 10.0
21 + 35 + 12 + 7
25 * 4 * 12
(3 * 5) + (10 - 6)
(3 * ((2 * 4) + (3 + 5))) + ((10 - 7) + 6)

# 1.1.2 The Elements of Programming - Naming and the Environment
size = 2
puts size
puts 5 * size
pi = 3.14159
radius = 10.0
puts pi * radius * radius
circumference = 2.0 * pi * radius
puts circumference

# 1.1.3 The Elements of Programming - Evaluating Combinations
puts (2 + (4 * 6)) * (3 + 5 + 7)

# 1.1.4 The Elements of Programming - Compound Procedures
def square(x) (x * x) end
puts square(21)
puts square(2 + 5)
puts square(square(3))
def sum_of_squares(x, y) square(x) + square(y) end
puts sum_of_squares(3, 4)
def f(a) sum_of_squares(a + 1, a * 2) end
puts f(5)

# 1.1.5 The Elements of Programming - The Substitution Model for Procedure Application
puts f(5)
puts sum_of_squares(5 + 1, 5 * 2)
puts square(6) + square(10)
puts (6 * 6) + (10 * 10)
puts 36 + 100
puts f(5)
puts sum_of_squares(5 + 1, 5 * 2)
puts square(5 + 1) + square(5 * 2)

puts ((5 + 1) * (5 + 1)) + ((5 * 2) * (5 * 2))
puts (6 * 6) + (10 * 10)
puts 36 + 100
puts 136

# 1.1.6 The Elements of Programming - Conditional Expressions and Predicates
def abs(x)
   if (x > 0)
      then x
      elsif (x == 0) then 0
      else -x
   end
end
def abs(x)
   if (x < 0)
      then -x
      else x
   end
end
x = 6
puts (x > 5) && (x < 10)
def ge(x, y)
   (x > y) || (x = y)
end
def ge(x, y)
   !(x < y)
end

# Exercise 1.1 #
puts 10
puts 5 + 3 + 4
puts 9 - 1
puts 6 / 2
puts (2 * 4) + (4 - 6)
a = 3
b = a + 1
puts a + b + (a * b)
puts (a == b)
puts (if ((b > a) && (b < (a *b))) then b else a end)
puts (if (a == 4) then 6 elsif (6 + 7 + a) then (b == 4) else 25 end)
puts 2 + (if (b > a) then b else a end)
puts (((a > b) && a) || ((a < b) && b) || -1) * (a + 1)

# Exercise 1.2 #
puts ((5.0 + 4.0 + (2.0 - (3.0 - (6.0 + (4.0 / 5.0))))) /
         (3.0 * (6.0 - 2.0) * (2.0 - 7.0)))

# Exercise 1.4 #
def a_plus_abs_b(a, b)
   if (b > 0)
      then a + b
      else a - b
   end
end

# Exercise 1.5 #
def p() p() end
def test(x, y)
   if (x == 0)
      then 0
      else y
   end
end
# commented out as this is in infinite loop
# test(0, p())

# 1.1.7 The Elements of Programming - Example: Square Roots by Newton's Method
def square(x) x * x end

def good_enough(guess, x)
   abs(square(guess) - x) < 0.001
end

def average(x, y)
   Float(x + y) / 2.0
end

def improve(guess, x)
   average(guess, Float(x) / guess)
end

def sqrt_iter(guess, x)
   if (good_enough(guess, x))
      then guess
      else sqrt_iter(improve(guess, x), x)
   end
end

def sqrt(x)
   sqrt_iter(1.0, Float(x))
end

puts sqrt(9.0)
puts sqrt(100.0 + 37.0)
puts sqrt(sqrt(2.0)+sqrt(3.0))
puts square(sqrt(1000.0))

# Exercise 1.6 #
def new_if(predicate, then_clause, else_clause)
   if (predicate)
      then then_clause
      else else_clause
   end
end
puts new_if((2==3), 0, 5)
puts new_if((1==1), 0, 5)
def sqrt_iter(guess, x)
   new_if(
      good_enough(guess, x),
      guess,
      sqrt_iter(improve(guess, x), x))
end

# 1.1.8 The Elements of Programming - Procedures as Black-Box Abstractions
def square(x) x * x end

def double(x) x + x end

def square_real(x) Math.exp(double(Math.log(x))) end

def good_enough(guess, x)
   abs(square(guess) - x) < 0.001
end

def improve(guess, x)
   average(guess, Float(x) / guess)
end

def sqrt_iter(guess, x)
   if (good_enough(guess, x))
      then guess
      else sqrt_iter(improve(guess, x), x)
   end
end

def sqrt(x)
   sqrt_iter(1.0, x)
end

puts square(5.0)

# Block-structured
def sqrt(x)
   def good_enough(guess, x)
      abs(square(guess) - x) < 0.001
   end

   def improve(guess, x)
      average(guess, Float(x) / guess)
   end

   def sqrt_iter(guess, x)
      if (good_enough(guess, x))
         then guess
         else sqrt_iter(improve(guess, x), x)
      end
   end

   sqrt_iter(1.0, x)
end

# Taking advantage of lexical scoping
# Not sure how to get lexical scoping for nested functions in Ruby.
# Will use lambdas to get around the problem
def sqrt(x)
   good_enough = lambda{|guess|
      abs(square(guess) - x) < 0.001
   }

   improve = lambda{|guess|
      average(guess, Float(x) / guess)
   }

   sqrt_iter = lambda{|guess|
      if (good_enough.call(guess))
         then guess
         else sqrt_iter.call(improve.call(guess))
      end
   }

   sqrt_iter.call(1.0)
end

# 1.2.1 Procedures and the Processes They Generate - Linear Recursion and Iteration

# Recursive
def factorial(n)
   if (n == 1)
      then 1
      else n * factorial(n - 1)
   end
end

# Iterative
def fact_iter(product, counter, max_count)
   if (counter > max_count)
      then product
      else fact_iter(counter * product, counter + 1, max_count)
   end
end

def factorial(n)
   fact_iter(1, 1, n)
end

# Iterative, block-structured (from footnote)
def factorial(n)
   # Note: sending n as parameter so I don't have to use lambda
   def iter(product, counter, n)
      if (counter > n)
         then product
         else iter(counter * product, counter + 1, n)
      end
   end
   iter(1, 1, n)
end

# Exercise 1.9 #
def inc(a) a + 1 end
def dec(a) a - 1 end
def plus(a, b)
   if (a == 0)
      then b
      else inc(dec(a) + b)
   end
end
def plus(a, b)
   if (a == 0)
      then b
      else dec(a) + inc(b)
   end
end

# Exercise 1.10 #
def a(x, y)
   if (y == 0)
      0
      elsif (x == 0) then 2 * y
      elsif (y == 1) then 2
      else a(x - 1, a(x, y - 1))
   end
end
puts a(1, 10)
puts a(2, 4)
puts a(3, 3)
def f(n) a(0, n) end
def g(n) a(1, n) end
def h(n) a(2, n) end
def k(n) 5 * n * n end
# 1.2.2 Procedures and the Processes They Generate - Tree Recursion

# Recursive
def fib(n)
   if (n == 0)
      then 0
      elsif (n == 1) then 1
      else fib(n - 1) + fib(n - 2)
   end
end

# Iterative
def fib_iter(a, b, count)
   if (count == 0)
      then b
      else fib_iter(a + b, a, count - 1)
   end
end
def fib(n)
   fib_iter(1, 0, n)
end

# Counting change
def first_denomination(x)
   if (x == 1)
      then 1
      elsif (x == 2) then 5
      elsif (x == 3) then 10
      elsif (x == 4) then 25
      elsif (x == 5) then 50
   end
end

def cc(amount, kinds_of_coins)
   if (amount == 0)
      then 1
      elsif (amount < 0) then 0
      elsif (kinds_of_coins == 0) then 0
      else (cc(amount, kinds_of_coins - 1) +
            cc(amount - first_denomination(kinds_of_coins), kinds_of_coins))
   end
end

def count_change(amount)
   cc(amount, 5)
end

puts count_change(100)

# Exercise 1.15 #
def cube(x) x * x * x end
def p(x) (3.0 * x) - (4.0 * cube(x)) end
def sine(angle)
   if !(abs(angle) > 0.1)
      then angle
      else p(sine(angle / 3.0))
   end
end

# 1.2.4 Procedures and the Processes They Generate - Exponentiation

# Linear recursion
def expt(b, n)
   if (n == 0)
      then 1
      else b * expt(b, (n - 1))
   end
end

# Linear iteration
def expt_iter(b, counter, product)
   if (counter == 0)
      then product
      else expt_iter(b, counter - 1, b * product)
   end
end
def expt(b, n)
   expt_iter(b, n, 1)
end

## Logarithmic iteration
def even(n) ((n % 2) == 0) end

def fast_expt(b, n)
   if (n == 0)
      then 1
      else
         if (even(n))
            then square(fast_expt(b, n / 2))
            else b * fast_expt(b, n - 1)
         end
   end
end

# Exercise 1.17 #
def multiply(a, b)
   if (b == 0)
      then 0
      else a + (a * (b - 1))
   end
end

# Exercise 1.19 #
# exercise left to reader to solve for p' and q'
#def fib_iter(a, b, p, q, count)
#   if (count == 0)
#      then b
#      else
#         if (even(count))
#            then fib_iter(a, b, p', q', count / 2)
#            else fib_iter((b * q) + (a * q) + (a * p), (b * p) + (a * q), p, q, count - 1)
#         end
#   end
#end
#def fib(n)
#   fib_iter(1, 0, 0, 1, n)
#end

# 1.2.5 Procedures and the Processes They Generate - Greatest Common Divisors
def gcd(a, b)
   if (b == 0)
      then a
      else gcd(b, a % b)
   end
end

# 1.2.6 Procedures and the Processes They Generate - Example: Testing for Primality

# prime
def divides(a, b) ((b % a) == 0) end

def find_divisor(n, test_divisor)
   if (square(test_divisor) > n)
      then n
      elsif (divides(test_divisor, n)) then test_divisor
      else find_divisor(n, test_divisor + 1)
   end
end

def smallest_divisor(n) find_divisor(n, 2) end

def prime(n) (n == smallest_divisor(n)) end

# fast_prime
def expmod(nbase, nexp, m)
   if (nexp == 0)
      then 1
      else
         if (even(nexp))
            then square(expmod(nbase, nexp / 2, m)) % m
            else (nbase * (expmod(nbase, (nexp - 1), m))) % m
         end
   end
end

def fermat_test(n)
   # Note: sending n as parameter so I don't have to use lambda
   def try_it(a, n) (expmod(a, n, n) == a) end
   try_it(rand(n), n)
end

def fast_prime(n, ntimes)
   if (ntimes == 0)
      then true
      else
         if (fermat_test(n))
            then fast_prime(n, ntimes - 1)
            else false
         end
   end
end

# Exercise 1.22 #
def report_prime(elapsed_time)
   puts " *** " + String(elapsed_time)
end
def start_prime_test(n, start_time)
   if (prime(n))
      report_prime(Time.now - start_time)
   end
end
def timed_prime_test(n)
   puts "\n" + String(n)
   start_prime_test(n, Time.now)
end

# Exercise 1.25 #
def expmod(nbase, nexp, m)
   fast_expt(nbase, nexp) % m
end

# Exercise 1.26 #
def expmod(nbase, nexp, m)
   if (nexp == 0)
      then 1
      else
         if (even(nexp))
            then (expmod(nbase, (nexp / 2), m) * expmod(nbase, (nexp / 2), m)) % m
            else (nbase * expmod(nbase, nexp - 1, m)) % m
         end
   end
end

# 1.3 Formulating Abstractions with Higher-Order Procedures
def cube(x) x * x * x end

# 1.3.1 Formulating Abstractions with Higher-Order Procedures - Procedures as Arguments
def sum_integers(a, b)
   if (a > b)
      then 0
      else a + sum_integers(a + 1, b)
   end
end

def sum_cubes(a, b)
   if (a > b)
      then 0
      else cube(a) + sum_cubes(a + 1, b)
   end
end

def pi_sum(a, b)
   if (a > b)
      then 0.0
      else (1.0 / (a * (a + 2.0))) + pi_sum(a + 4.0, b)
   end
end

def sum(term, a, nxt, b)
   if (a > b)
      then 0
      else term.call(a) + sum(term, nxt.call(a), nxt, b)
   end
end

# Using sum
def inc(n) n + 1 end

def sum_cubes(a, b)
   sum(lambda{|x| cube(x)}, a, lambda{|y| inc(y)}, b)
end

puts sum_cubes(1, 10)

def identity(x) x end

def sum_integers(a, b)
   sum(lambda{|x| identity(x)}, a, lambda{|y| inc(y)}, b)
end

puts sum_integers(1, 10)

def pi_sum(a, b)
   def pi_term(x) 1.0 / (x * (x + 2.0)) end
   def pi_next(x) x + 4.0 end
   sum(lambda{|x| pi_term(x)}, a, lambda{|y| pi_next(y)}, b)
end

puts 8.0 * pi_sum(1, 1000)

def integral(f, a, b, dx)
   add_dx = lambda{|x| x + dx}
   sum(f, a + (dx / 2.0), lambda{|y| add_dx.call(y)}, b) * dx
end

def cube(x) x * x * x end

puts integral(lambda{|x| cube(x)}, 0.0, 1.0, 0.01)
# exceeds maximum recursion depth
# puts integral(lambda{|x| cube(x)}, 0.0, 1.0, 0.001)

# Exercise 1.32 #
# exercise left to reader to define appropriate functions
# puts accumulate(combiner, null_value, term, a, next, b)

# 1.3.2 Formulating Abstractions with Higher-Order Procedures - Constructing Procedures Using Lambda
def pi_sum(a, b)
  sum(lambda{|x| 1.0 / (x * (x + 2.0))}, a, lambda{|y| y + 4.0}, b)
end

def integral(f, a, b, dx)
   sum(f, a + (dx / 2.0), lambda{|x| x + dx}, b) * dx
end

def plus4(x) x + 4 end

plus4 = lambda{|x| x + 4}

puts lambda{|x,y,z| x + y + square(z)}.call(1, 2, 3)

# Using let
def f(x, y)
   f_helper = lambda{|a, b|
      (x * square(a)) + (y * b) + (a * b)
   }
   f_helper.call(1 + (x * y), 1 - y)
end

def f(x, y)
   lambda{|a,b| (x * square(a)) + (y * b) + (a * b)}.call(1 + (x * y), 1 - y)
end

def f(x, y)
   a = 1 + (x * y)
   b = 1 - y
   (x * square(a)) + (y * b) + (a * b)
end

# Ruby does not have let binding and lambdas can't seem to be scoped
# There is a proposal in 1.9 to have local scoping:
#     http://eigenclass.org/hiki.rb?Changes+in+Ruby+1.9#l7
# Note: Ruby gives incorrect result here of 36 (different than Scheme)
x = 5
puts lambda{x = 3; x + x*10}.call() + x

# Note: have to declare y first here
x = 2
puts lambda{y=x+2; x=3; x*y}.call()

def f(x, y)
   a = 1 + (x * y)
   b = 1 - y
   (x + square(a)) + (y * b) + (a * b)
end

# Exercise 1.34 #
def f(g) g.call(2) end
puts f(lambda{|x| square(x)})
puts f(lambda{|z| z * (z + 1)})

# 1.3.3 Formulating Abstractions with Higher-Order Procedures - Procedures as General Methods

# Half-interval method
def close_enough(x, y)
   (abs(x - y) < 0.001)
end

def positive(x) (x >= 0.0) end
def negative(x) !(positive(x)) end

def search(f, neg_point, pos_point)
   midpoint = average(neg_point, pos_point)
   if (close_enough(neg_point, pos_point))
      then midpoint
      else
         begin
            test_value = f.call(midpoint)
            if (positive(test_value))
               then search(f, neg_point, midpoint)
               elsif (negative(test_value)) then search(f, midpoint, pos_point)
               else midpoint
            end
         end
   end
end

def half_interval_method(f, a, b)
   a_value = f.call(a)
   b_value = f.call(b)
   if (negative(a_value) && positive(b_value))
      then search(f, a, b)
      elsif (negative(b_value) && positive(a_value)) then search(f, b, a)
      else raise Exception.new("Values are not of opposite sign " + String(a) + " " + String(b))
   end
end

puts half_interval_method(lambda{|x| Math.sin(x)}, 2.0, 4.0)

puts half_interval_method(lambda{|x| (x * x * x) - (2.0 * x) - 3.0}, 1.0, 2.0)

## Fixed points
Tolerance = 0.00001

def fixed_point(f, first_guess)
   def close_enough(v1, v2)
      abs(v1 - v2) < Tolerance
   end
   # Note: sending f as parameter so I don't have to use lambda
   def tryit(guess, f)
      nxt = f.call(guess)
      if (close_enough(guess, nxt))
         then nxt
         else tryit(nxt, f)
      end
   end
   tryit(first_guess, f)
end

puts fixed_point(lambda{|x| Math.cos(x)}, 1.0)

puts fixed_point(lambda{|y| Math.sin(y) + Math.cos(y)}, 1.0)

# note: this function does not converge
def sqrt(x)
   fixed_point(lambda{|y| Float(x) / y}, 1.0)
end

def sqrt(x)
   fixed_point(lambda{|y| average(y, Float(x) / y)}, 1.0)
end

# Exercise 1.37 #
# exercise left to reader to define cont_frac
# cont_frac(lambda{|i| 1.0}, lambda{|j| 1.0}, k)

# 1.3.4 Formulating Abstractions with Higher-Order Procedures - Procedures as Returned Values
def average_damp(f)
   lambda{|x| average(Float(x), f.call(x)) }
end

puts average_damp(lambda{|x| square(x)}).call(10.0)

def sqrt(x)
   fixed_point(average_damp(lambda{|y| Float(x) / y}), 1.0)
end

def cube_root(x)
   fixed_point(average_damp(lambda{|y| Float(x) / square(y)}), 1.0)
end

puts cube_root(8)

# Newton's method
def deriv(g)
   dx = 0.00001
   lambda{|x| Float(g.call(x + dx) - g.call(x)) / dx}
end

def cube(x) x * x * x end

puts deriv(lambda{|x| cube(x)}).call(5.0)

def newton_transform(g)
   lambda{|x| x - (Float(g.call(x)) / (deriv(g).call(x)))}
end

def newtons_method(g, guess)
   fixed_point(newton_transform(g), guess)
end

def sqrt(x)
   newtons_method(lambda{|y| square(y) - x}, 1.0)
end

# Fixed point of transformed function
def fixed_point_of_transform(g, transform, guess)
   fixed_point(transform.call(g), guess)
end

def sqrt(x)
   fixed_point_of_transform(lambda{|y| puts x; puts y; (x / y)}, lambda{|z| average_damp(z)}, 1.0)
end

def sqrt(x)
   fixed_point_of_transform(lambda{|y| square(y) - x}, lambda{|z| newton_transform(z)}, 1.0)
end

# Exercise 1.40 #
# exercise left to reader to define cubic
# puts newtons_method(lambda{|a, b, c| cubic(a, b, c)}, 1.0)

# Exercise 1.41 #
# exercise left to reader to define double
# puts (double.call(double.call(double))).call(inc).call(5)

# Exercise 1.42 #
# exercise left to reader to define compose
# puts (compose(lambda{|x| square(x)}, lambda{|y| inc(y)})).call(6)

# Exercise 1.43 #
# exercise left to reader to define repeated
# puts (repeated({lambda{|x| square(x)}, 2)).call(5)
