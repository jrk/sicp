-- 1.1.1 The Elements of Programming - Expressions
print (486)
print (137 + 349)
print (1000 - 334)
print (5 * 99)
print (10 / 5)
print (2.7 + 10)
print (21 + 35 + 12 + 7)
print (25 * 4 * 12)
print ((3 * 5) + (10 - 6))
print ((3 * ((2 * 4) + (3 + 5))) + ((10 - 7) + 6))

-- 1.1.2 The Elements of Programming - Naming and the Environment
size = 2
print (size)
print (5 * size)
pi = 3.14159
radius = 10
print (pi * radius * radius)
circumference = 2 * pi * radius
print (circumference)

-- 1.1.3 The Elements of Programming - Evaluating Combinations
print ((2 + (4 * 6)) * (3 + 5 + 7))

-- 1.1.4 The Elements of Programming - Compound Procedures
function square(x) return (x * x) end
print (square(21))
print (square(2 + 5))
print (square(square(3)))
function sum_of_squares(x, y) return square(x) + square(y) end
print (sum_of_squares(3, 4))
function f(a) return sum_of_squares(a + 1, a * 2) end
print (f(5))

-- 1.1.5 The Elements of Programming - The Substitution Model for Procedure Application
print (f(5))
print (sum_of_squares(5 + 1, 5 * 2))
print (square(6) + square(10))
print ((6 * 6) + (10 * 10))
print (36 + 100)
print (f(5))
print (sum_of_squares(5 + 1, 5 * 2))
print (square(5 + 1) + square(5 * 2))

print (((5 + 1) * (5 + 1)) + ((5 * 2) * (5 * 2)))
print ((6 * 6) + (10 * 10))
print (36 + 100)
print (136)

-- 1.1.6 The Elements of Programming - Conditional Expressions and Predicates
function abs(x)
   if (x > 0) then
      return x
   elseif (x == 0) then
      return 0
   else
      return -x
   end
end
function abs(x)
   if (x < 0) then
      return -x
   else
      return x
   end
end
x = 6
print ((x > 5) and (x < 10))
function ge(x, y)
   return (x > y) or (x == y)
end
function ge(x, y)
   return not(x < y)
end

-- Exercise 1.1 --
print (10)
print (5 + 3 + 4)
print (9 - 1)
print (6 / 2)
print ((2 * 4) + (4 - 6))
a = 3
b = a + 1
print (a + b + (a * b))
print ((a == b))
print (((b > a) and (b < (a * b))) and b or a)
print ((a == 4) and 6 or (b == 4) and (6 + 7 + a) or 25)
print (2 +  ((b > a) and b or a))
print (((a > b) and a or (a < b) and b or -1 ) * (a + 1))

-- Exercise 1.2 --
print (((5 + 4 + (2 - (3 - (6 + (4 / 5))))) /
        (3 * (6 - 2) * (2 - 7))))

-- Exercise 1.4 --
function a_plus_abs_b(a, b)
   if (b > 0) then
      return a + b
   else
      return a - b
   end
end

-- Exercise 1.5 --
function p() return p() end
function test(x, y)
   if (x == 0) then
      return 0
   else
      return y
   end
end
-- commented out as this is in infinite loop
-- test(0, p())

-- 1.1.7 The Elements of Programming - Example: Square Roots by Newton's Method
function square(x) return x * x end

function good_enough(guess, x)
   return abs(square(guess) - x) < 0.001
end

function average(x, y)
   return (x + y) / 2.0
end

function improve(guess, x)
   return average(guess, x / guess)
end

function sqrt_iter(guess, x)
   if (good_enough(guess, x)) then
      return guess
   else
      return sqrt_iter(improve(guess, x), x)
   end
end

function sqrt(x)
   return sqrt_iter(1.0, x)
end

print (sqrt(9))
print (sqrt(100 + 37))
print (sqrt(sqrt(2)+sqrt(3)))
print (square(sqrt(1000)))

-- Exercise 1.6 --
function new_if(predicate, then_clause, else_clause)
   if (predicate) then
      return then_clause
   else
      return else_clause
   end
end
print (new_if((2==3), 0, 5))
print (new_if((1==1), 0, 5))
function sqrt_iter(guess, x)
   return new_if(
      good_enough(guess, x),
      guess,
      sqrt_iter(improve(guess, x), x))
end

-- 1.1.8 The Elements of Programming - Procedures as Black-Box Abstractions
function square(x) return x * x end

function double(x) return x + x end

function square_real(x) return exp(double(math.log(x))) end

function good_enough(guess, x)
   return abs(square(guess) - x) < 0.001
end

function improve(guess, x)
   return average(guess, x / guess)
end

function sqrt_iter(guess, x)
   if (good_enough(guess, x)) then
      return guess
   else
      return sqrt_iter(improve(guess, x), x)
   end
end

function sqrt(x)
   return sqrt_iter(1.0, x)
end

print (square(5))

-- Block-structured
function sqrt(x)
   local function good_enough(guess, x)
      return abs(square(guess) - x) < 0.001
   end

   local function improve(guess, x)
      return average(guess, x / guess)
   end

   local function sqrt_iter(guess, x)
      if (good_enough(guess, x)) then
         return guess
      else
         return sqrt_iter(improve(guess, x), x)
      end
   end

   return sqrt_iter(1.0, x)
end

-- Taking advantage of lexical scoping
function sqrt(x)
   local function good_enough(guess)
      return abs(square(guess) - x) < 0.001
   end

   local function improve(guess)
      return average(guess, x / guess)
   end

   local function sqrt_iter(guess)
      if (good_enough(guess)) then
         return guess
      else
         return sqrt_iter(improve(guess))
      end
   end

   return sqrt_iter(1.0)
end

-- 1.2.1 Procedures and the Processes They Generate - Linear Recursion and Iteration

-- Recursive
function factorial(n)
   if (n == 1) then
      return 1
   else
      return n * factorial(n - 1)
   end
end

-- Iterative
function fact_iter(product, counter, max_count)
   if (counter > max_count) then
      return product
   else
      return fact_iter(counter * product, counter + 1, max_count)
   end
end

function factorial(n)
   return fact_iter(1, 1, n)
end

-- Iterative, block-structured (from footnote)
function factorial(n)
   local function iter(product, counter)
      if (counter > n)
         then return product
      else
         return iter(counter * product, counter + 1)
      end
   end
   return iter(1, 1)
end

-- Exercise 1.9 --
function inc(a) return a + 1 end
function dec(a) return a - 1 end
function plus(a, b)
   if (a == 0) then
      return b
   else
      return inc(dec(a) + b)
   end
end
function plus(a, b)
   if (a == 0) then
      return b
   else
      return dec(a) + inc(b)
   end
end

-- Exercise 1.10 --
function a(x, y)
   if (y == 0) then
      return 0
   elseif (x == 0) then
      return 2 * y
   elseif (y == 1) then
      return 2
   else
      return a(x - 1, a(x, y - 1))
   end
end
print (a(1, 10))
print (a(2, 4))
print (a(3, 3))
function f(n) return a(0, n) end
function g(n) return a(1, n) end
function h(n) return a(2, n) end
function k(n) return 5 * n * n end

-- 1.2.2 Procedures and the Processes They Generate - Tree Recursion

-- Recursive
function fib(n)
   if (n == 0) then
      return 0
   elseif (n == 1) then
      return 1
   else
      return fib(n - 1) + fib(n - 2)
   end
end

-- Iterative
function fib_iter(a, b, count)
   if (count == 0) then
      return b
   else
      return fib_iter(a + b, a, count - 1)
   end
end
function fib(n)
   return fib_iter(1, 0, n)
end

-- Counting change
function first_denomination(x)
   if (x == 1) then
      return 1
   elseif (x == 2) then
      return 5
   elseif (x == 3) then
      return 10
   elseif (x == 4) then
      return 25
   elseif (x == 5) then
      return 50
   end
end

function cc(amount, kinds_of_coins)
   if (amount == 0) then
      return 1
   elseif (amount < 0) then
      return 0
   elseif (kinds_of_coins == 0) then
      return 0
   else
      return (cc(amount, kinds_of_coins - 1) +
              cc(amount - first_denomination(kinds_of_coins), kinds_of_coins))
   end
end

function count_change(amount)
   return cc(amount, 5)
end

print (count_change(100))

-- Exercise 1.15 --
function cube(x) return x * x * x end
function p(x) return (3.0 * x) - (4.0 * cube(x)) end
function sine(angle)
   if not(abs(angle) > 0.1) then
      return angle
   else
      return p(sine(angle / 3.0))
   end
end

-- 1.2.4 Procedures and the Processes They Generate - Exponentiation

-- Linear recursion
function expt(b, n)
   if (n == 0) then
      return 1
   else
      return b * expt(b, (n - 1))
   end
end

-- Linear iteration
function expt_iter(b, counter, product)
   if (counter == 0) then
      return product
   else
      return expt_iter(b, counter - 1, b * product)
   end
end
function expt(b, n)
   return expt_iter(b, n, 1)
end

-- Logarithmic iteration
function even(n) return (math.mod(n, 2) == 0) end

function fast_expt(b, n)
   if (n == 0) then
      return 1
   else
      if (even(n)) then
         return square(fast_expt(b, n / 2))
      else
         return b * fast_expt(b, n - 1)
      end
   end
end

-- Exercise 1.17 --
function multiply(a, b)
   if (b == 0) then
      return 0
   else
      return a + (a * (b - 1))
   end
end

-- Exercise 1.19 --
-- exercise left to reader to solve for p' and q'
function fib_iter(a, b, p, q, count)
   if (count == 0) then
      return b
   else
      if (even(count)) then
         return fib_iter(a, b, p_, q_, count / 2)
      else
         return fib_iter((b * q) + (a * q) + (a * p), (b * p) + (a * q), p, q, count - 1)
      end
   end
end
function fib(n)
   return fib_iter(1, 0, 0, 1, n)
end


-- 1.2.5 Procedures and the Processes They Generate - Greatest Common Divisors
function gcd(a, b)
   if (b == 0) then
      return a
   else
      return gcd(b, math.mode(a, b))
   end
end

-- 1.2.6 Procedures and the Processes They Generate - Example: Testing for Primality

-- prime
function divides(a, b) return (math.mod(b, a) == 0) end

function find_divisor(n, test_divisor)
   if (square(test_divisor) > n) then
      return n
   elseif (divides(test_divisor, n)) then
      return test_divisor
   else
      return find_divisor(n, test_divisor + 1)
   end
end

function smallest_divisor(n)
   return find_divisor(n, 2)
end

function prime(n)
   return (n == smallest_divisor(n))
end

-- fast_prime
function expmod(nbase, nexp, m)
   if (nexp == 0) then
      return 1
   else
      if (even(nexp)) then
         return math.mod(square(expmod(nbase, nexp / 2, m)), m)
      else
         return math.mod(nbase * (expmod(nbase, (nexp - 1), m)), m)
      end
   end
end

function fermat_test(n)
   local function try_it(a)
      return (expmod(a, n, n) == a)
   end
   return try_it(1 + random.int(0, n-1))
end

function fast_prime(n, ntimes)
   if (ntimes == 0) then
      return true
   else
      if (fermat_test(n)) then
         return fast_prime(n, ntimes - 1)
      else
         return false
      end
   end
end

-- Exercise 1.22 --
function report_prime(elapsed_time)
   print (string.format(" *** %f", elapsed_time))
end
function start_prime_test(n, start_time)
   if (prime(n)) then
      report_prime(os.clock() - start_time)
   end
end
function timed_prime_test(n)
   print (string.format("\n%d", n))
   start_prime_test(n, os.clock())
end

-- Exercise 1.25 --
function expmod(nbase, nexp, m)
   return math.mod(fast_expt(nbase, nexp), m)
end

-- Exercise 1.26 --
function expmod(nbase, nexp, m)
   if (nexp == 0) then
      return 1
   else
      if (even(nexp)) then
         return math.mod(expmod(nbase, (nexp / 2), m) * expmod(nbase, (nexp / 2), m), m)
      else
         return math.mod(nbase * expmod(nbase, nexp - 1, m), m)
      end
   end
end

-- 1.3 Formulating Abstractions with Higher-Order Procedures
function cube(x) return x * x * x end

-- 1.3.1 Formulating Abstractions with Higher-Order Procedures - Procedures as Arguments
function sum_integers(a, b)
   if (a > b) then
      return 0
   else
      return a + sum_integers(a + 1, b)
   end
end

function sum_cubes(a, b)
   if (a > b) then
      return 0
   else
      return cube(a) + sum_cubes(a + 1, b)
   end
end

function pi_sum(a, b)
   if (a > b) then
      return 0.0
   else
      return (1.0 / (a * (a + 2.0))) + pi_sum(a + 4.0, b)
   end
end

function sum(term, a, next, b)
   if (a > b) then
      return 0
   else
      return term(a) + sum(term, next(a), next, b)
   end
end

-- Using sum
function inc(n) return n + 1 end

function sum_cubes(a, b)
   return sum(cube, a, inc, b)
end

print (sum_cubes(1, 10))

function identity(x) return x end

function sum_integers(a, b)
   return sum(identity, a, inc, b)
end

print (sum_integers(1, 10))

function pi_sum(a, b)
   local function pi_term(x) return 1.0 / (x * (x + 2.0)) end
   local function pi_next(x) return x + 4.0 end
   return sum(pi_term, a, pi_next, b)
end

print (8.0 * pi_sum(1, 1000))

function integral(f, a, b, dx)
   local function add_dx(x) return x + dx end
   return sum(f, a + (dx / 2.0), add_dx, b) * dx
end

function cube(x) return x * x * x end

print (integral(cube, 0.0, 1.0, 0.01))
print (integral(cube, 0.0, 1.0, 0.001))

-- Exercise 1.32 --
-- exercise left to reader to define appropriate functions
-- print (accumulate(combiner, null_value, term, a, next, b))

-- 1.3.2 Formulating Abstractions with Higher-Order Procedures - Constructing Procedures Using Lambda
function pi_sum(a, b)
   return sum(
      function(x) return 1.0 / (x * (x + 2.0)) end,
      a,
      function(x) return x + 4.0 end,
      b)
end

function integral(f, a, b, dx)
   return sum(
      f,
      a + (dx / 2.0),
      function(x) return x + dx end,
      b) * dx
end

function plus4(x) return x + 4 end

plus4 = function(x) return x + 4 end

print ((function(x, y, z) return x + y + square(z) end) (1, 2, 3))

-- Using let
function f(x, y)
   local function f_helper(a, b)
      return (x * square(a)) + (y * b) + (a * b)
   end
   return f_helper(1 + (x * y), 1 - y)
end

-- not sure why need to go through a variable here???
function f(x, y)
   return
      (function(a, b)
         return (x * square(a)) + (y * b) + (a * b)
       end) (1 + (x * y), 1 - y)
end

function f(x, y)
   local a = 1 + (x * y)
   local b = 1 - y
   return (x * square(a)) + (y * b) + (a * b)
end

-- lua does not seem to have let binding.  use function to demo scoping.
x = 5
print ((function()
         local x = 3
         return x + (x * 10)
        end)() + x)

x = 2
print ((function(x)
         local y = x + 2
         local x = 3
         return x * y
        end)(x))

function f(x, y)
   local a = 1 + (x * y)
   local b = 1 - y
   return (x + square(a)) + (y * b) + (a * b)
end

-- Exercise 1.34 --
function f(g) return g(2) end
print (f(square))
print (f(function(z) return z * (z + 1) end))

-- 1.3.3 Formulating Abstractions with Higher-Order Procedures - Procedures as General Methods

-- Half-interval method
function close_enough(x, y)
   return (abs(x - y) < 0.001)
end

function positive(x) return (x >= 0.0) end
function negative(x) return not(positive(x)) end

function search(f, neg_point, pos_point)
   local midpoint = average(neg_point, pos_point)
   if (close_enough(neg_point, pos_point)) then
      return midpoint
   else
      local test_value = f(midpoint)
      if (positive(test_value)) then
         return search(f, neg_point, midpoint)
      elseif (negative(test_value)) then
         return search(f, midpoint, pos_point)
      else
         return midpoint
      end
   end
end

function half_interval_method(f, a, b)
   local a_value = f(a)
   local b_value = f(b)
   if (negative(a_value) and positive(b_value)) then
      return search(f, a, b)
   elseif (negative(b_value) and positive(a_value)) then
      return search(f, b, a)
   else
      error(string.format("Values are not of opposite sign %d %d", a, b))
   end
end

print (half_interval_method(math.sin, 2.0, 4.0))

print (half_interval_method(function(x) return (x * x * x) - (2.0 * x) - 3.0 end, 1.0, 2.0))

-- Fixed points
tolerance = 0.00001

function fixed_point(f, first_guess)
   local function close_enough(v1, v2)
      return abs(v1 - v2) < tolerance
   end
   local function tryit(guess)
      local next = f(guess)
      if (close_enough(guess, next)) then
         return next
      else
         return tryit(next)
      end
   end
   return tryit(first_guess)
end

print (fixed_point(math.cos, 1.0))

print (fixed_point(function(y) return math.sin(y) + math.cos(y) end, 1.0))

-- note: this function does not converge
function sqrt(x)
   return fixed_point(function(y) return x / y end, 1.0)
end

function sqrt(x)
   return fixed_point(function(y) return average(y, x / y) end, 1.0)
end

-- Exercise 1.37 --
-- exercise left to reader to define cont_frac
-- cont_frac(function(i) return 1.0 end, lambda(i) return 1.0 end, k)

-- 1.3.4 Formulating Abstractions with Higher-Order Procedures - Procedures as Returned Values
function average_damp(f)
   return (function(x) return average(x, f(x)) end)
end

print ((average_damp(square)) (10.0))

function sqrt(x)
   return fixed_point(average_damp(function(y) return x / y end), 1.0)
end

function cube_root(x)
   return fixed_point(average_damp(function(y) return x / square(y) end), 1.0)
end

print (cube_root(8))

-- Newton's method
dx = 0.00001
function deriv(g)
   return (function(x) return (g(x + dx) - g(x)) / dx end)
end

function cube(x) return x * x * x end

print (deriv(cube) (5.0))

function newton_transform(g)
   return (function(x) return x - (g(x) / (deriv(g) (x))) end)
end

function newtons_method(g, guess)
   return fixed_point(newton_transform(g), guess)
end

function sqrt(x)
   return newtons_method(function(y) return square(y) - x end, 1.0)
end

-- Fixed point of transformed function
function fixed_point_of_transform(g, transform, guess)
   return fixed_point(transform(g), guess)
end

function sqrt(x)
   return fixed_point_of_transform(function(y) return x / y end, average_damp, 1.0)
end

function sqrt(x)
   return
      fixed_point_of_transform(
         function(y) return square(y) - x end,
         newton_transform,
         1.0)
end

-- Exercise 1.40 --
-- exercise left to reader to define cubic
-- print (newtons_method(cubic(a, b, c), 1.0))

-- Exercise 1.41 --
-- exercise left to reader to define double
-- print ((double(double(double)))(inc) (5))

-- Exercise 1.42 --
-- exercise left to reader to define compose
-- print ((compose(square, inc)) (6))

-- Exercise 1.43 --
-- exercise left to reader to define repeated
-- print ((repeated(square, 2)) (5))
