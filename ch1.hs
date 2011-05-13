module SICP01() where

-- 1.1.1 The Elements of Programming - Expressions
_ = 486
_ = 137 + 349
_ = 1000 - 334
_ = 5 * 99
_ = 10 `div` 5
_ = 2.7 + 10
_ = 21 + 35 + 12 + 7
_ = 25 * 4 * 12
_ = (3 * 5) + (10 - 6)
_ = (3 * ((2 * 4) + (3 + 5))) + ((10 - 7) + 6)

-- 1.1.2 The Elements of Programming - Naming and the Environment
size = 2
_ = size
_ = 5 * size
pi' = 3.14159
radius = 10
_ = pi' * radius * radius
circumference = 2.0 * pi' * radius
_ = circumference

-- 1.1.3 The Elements of Programming - Evaluating Combinations
_ = (2 + (4 * 6)) * (3 + 5 + 7)

-- 1.1.4 The Elements of Programming - Compound Procedures
square x = x * x
_ = square 21
_ = square (2 + 5)
_ = square (square 3)
sum_of_squares x y = (square x) + (square y)
_ = sum_of_squares 3 4
f a = sum_of_squares (a + 1) (a * 2)
_ = f 5

-- 1.1.5 The Elements of Programming - The Substitution Model for Procedure Application
_ = f 5
_ = sum_of_squares (5 + 1) (5 * 2)
_ = (square 6) + (square 10)
_ = (6 * 6) + (10 * 10)
_ = 36 + 100
_ = f 5
_ = sum_of_squares (5 + 1) (5 * 2)
_ = (square (5 + 1)) + (square (5 * 2))

_ = ((5 + 1) * (5 + 1)) + ((5 * 2) * (5 * 2))
_ = (6 * 6) + (10 * 10)
_ = 36 + 100
_ = 136

-- 1.1.6 The Elements of Programming - Conditional Expressions and Predicates
abs' x =
   if (x > 0)
      then x
      else if (x == 0)
         then 0
         else -x
abs_1 x =
   if (x < 0)
      then -x
      else x
x = 6
_ = (x > 5) && (x < 10)
ge x y =
   (x > y) || (x == y)
ge' x y =
   not (x < y)

-- Exercise 1.1
_ = 10
_ = 5 + 3 + 4
_ = 9 - 1
_ = 6 `div` 2
_ = (2 * 4) + (4 - 6)
a = 3
b = a + 1
_ = a + b + (a * b)
_ = (a == b)
_ = if (b > a) && (b < (a * b))
      then b
      else a
_ = if (a == 4)
      then 6
      else
         if (b == 4)
            then 6 + 7 + a
            else 25
_ = 2 + (if (b > a) then b else a)
_ = (if (a > b)
      then a
      else if (a < b)
         then b
         else -1) * (a + 1)

-- Exercise 1.2
_ = (5 + 4 + (2 - (3 - (6 + (4 / 5))))) /
      (3 * (6 - 2) * (2 - 7))

-- Exercise 1.4
a_plus_abs_b a b =
   if (b > 0)
      then a + b
      else a - b

-- Exercise 1.5
p () = p()
test x y =
   if (x == 0)
      then 0
      else y
-- commented out as this is in infinite loop
-- test 0 p()

-- 1.1.7 The Elements of Programming - Example: Square Roots by Newton's Method
square' x = x * x

good_enough guess x =
   abs ((square' guess) - x) < 0.001

average x y =
   (x + y) / 2

improve guess x =
   average guess (x / guess)

sqrt_iter guess x =
   if good_enough guess x
      then guess
      else sqrt_iter (improve guess x) x

sqrt' x =
   sqrt_iter 1.0 x

_ = sqrt' 9
_ = sqrt' (100 + 37)
_ = sqrt' ((sqrt' 2)+(sqrt' 3))
_ = square' (sqrt' 1000)

-- Exercise 1.6
new_if predicate then_clause else_clause =
   if (predicate)
      then then_clause
      else else_clause
_ = new_if (2==3) 0 5
_ = new_if (1==1) 0 5
sqrt_iter' guess x =
   new_if (good_enough guess x)
      guess
      (sqrt_iter' (improve guess x) x)

-- 1.1.8 The Elements of Programming - Procedures as Black-Box Abstractions
square_1 x = x * x

double x = x + x

square_2 x = exp(double(log x))

good_enough' guess x =
   abs ((square guess) - x) < 0.001

improve' guess x =
   average guess (x / guess)

sqrt_iter_1 guess x =
   if (good_enough' guess x)
      then guess
      else sqrt_iter_1 (improve guess x) x

sqrt_1 x =
   sqrt_iter_1 1.0 x

_ = square 5.0

-- Block-structured
sqrt_2 x =
   let
      good_enough guess x =
         abs ((square guess) - x) < 0.001

      improve guess x =
         average guess (x / guess)

      sqrt_iter guess x =
         if good_enough guess x
            then guess
            else sqrt_iter (improve guess x) x
   in
      sqrt_iter 1.0 x

-- Taking advantage of lexical scoping
sqrt_3 x =
   let
      good_enough guess =
         abs((square guess) - x) < 0.001

      improve guess =
         average guess (x / guess)

      sqrt_iter guess =
         if (good_enough guess)
            then guess
            else sqrt_iter (improve guess)
   in
      sqrt_iter 1.0


-- 1.2.1 Procedures and the Processes They Generate - Linear Recursion and Iteration

-- Recursive
factorial n =
   if n == 1
      then 1
      else n * factorial (n - 1)

-- Iterative
factorial' n = fact_iter 1 1 n

fact_iter product counter max_count =
   if counter > max_count
      then product
      else fact_iter (counter * product) (counter + 1) max_count

-- Iterative, block-structured (from footnote)
factorial_1 n = iter 1 1
   where
      iter product counter =
         if counter > n
            then product
            else iter (counter * product) (counter + 1)

-- Exercise 1.9
inc a = a + 1
dec a = a - 1
plus a b =
   if (a == 0)
      then b
      else inc ((dec a) + b)
plus' a b =
   if (a == 0)
      then b
      else (dec a) + (inc b)

-- Exercise 1.10
a' x y =
   case (x, y) of
      (x, 0) -> 0
      (0, y) -> 2 * y
      (x, 1) -> 2
      (x, y) -> a' (x - 1) (a' x (y - 1))
_ = a' 1 10
_ = a' 2 4
_ = a' 3 3
f' n = a' 0 n
g' n = a' 1 n
h' n = a' 2 n
k' n = 5 * n * n


-- 1.2.2 Procedures and the Processes They Generate - Tree Recursion

-- Recursive
fib n =
   case n of
      0 -> 0
      1 -> 1
      _ -> fib (n - 1) + fib (n - 2)

-- Iterative
fib' n = fib_iter 1 0 n

fib_iter a b count =
   if count == 0
      then b
      else fib_iter (a + b) a (count -1)

-- Counting change
count_change amount = cc amount 5

cc amount kinds_of_coins
   | amount == 0                       = 1
   | amount < 0 || kinds_of_coins == 0 = 0
   | otherwise                         =
      (cc (amount - (first_denomination kinds_of_coins)) kinds_of_coins) +
         (cc amount (kinds_of_coins - 1))

first_denomination kinds_of_coins =
   case kinds_of_coins of
      1 -> 1
      2 -> 5
      3 -> 10
      4 -> 25
      5 -> 50

_ = count_change 100

-- Exercise 1.15
cube x = x * x * x
p' x = (3.0 * x) - (4.0 * (cube x))
sine angle =
   if not(abs angle > 0.1)
      then angle
      else p' (sine(angle / 3.0))


-- 1.2.4 Procedures and the Processes They Generate - Exponentiation

-- Linear recursion
exp' b n =
    if n == 0
       then 1
       else b * (exp' b  (n -1))

-- Linear iteration
exp_iter b counter product =
    if counter == 0
       then product
       else exp_iter b (counter - 1) (b * product)

exp_1 b n = exp_iter b n 1

even' n = ((n `mod` 2) == 0)

fast_exp b n
   | n == 0 = 1
   | even n = square (fast_exp b (n `div` 2))
   | otherwise = b * fast_exp b (n - 1)


-- Logarithmic iteration
fast_expt b n
   | n == 0 = 1
   | otherwise =
      if (even n)
         then square (fast_expt b (n `div` 2))
         else b * fast_expt b (n - 1)

-- Exercise 1.17
multiply a b
   | b == 0 = 0
   | otherwise = a + (a * (b - 1))

-- Exercise 1.19
-- exercise left to reader to solve for p' and q'
--    fib_iter' a b p q count
--         | count == 0 = b
--         | otherwise =
--            if (even count)
--               then fib_iter' a b p' q' (count `div` 2)
--               else fib_iter' ((b * q) + (a * q) + (a * p)) ((b * p) + (a * q)) p q (count - 1)
--    fib_1 n =
--         fib_iter' 1 0 0 1 n


-- 1.2.5 Procedures and the Processes They Generate - Greatest Common Divisors
gcd' a b
   | b == 0 = a
   | otherwise = gcd' b (a `mod` b)


-- 1.2.6 Procedures and the Processes They Generate - Example: Testing for Primality

-- prime
divides a b = ((b `mod` a) == 0)

find_divisor n test_divisor =
   if ((square test_divisor) > n)
      then n
      else
         if divides test_divisor n
            then test_divisor
            else find_divisor n (test_divisor + 1)

smallest_divisor n = find_divisor n 2

prime n = (n == smallest_divisor n)

-- fast_prime
expmod nbase nexp m
   | nexp == 0 = 1
   | otherwise =
      if (even nexp)
         then (square (expmod nbase (nexp `div` 2) m)) `mod` m
         else (nbase * (expmod nbase (nexp - 1) m)) `mod` m

-- ??? need to figure out how to use the Random.random function
fermat_test n =
   let
      try_it a = ((expmod a n n) == a)
   in
      try_it (1 + 2)                            -- Random.random(n - 1)) ???

fast_prime n ntimes
   | ntimes == 0 = True
   | otherwise =
      if (fermat_test n)
         then fast_prime n (ntimes - 1)
         else False

-- Exercise 1.22
report_prime elapsed_time =
   let
      _ = putStrLn (" *** " ++ (show elapsed_time))
   in
      ()

-- ??? need to figure out how to get time in milliseconds
get_time_in_milliseconds () = 1000      -- Time.toMilliseconds(Time.now()) ???

start_prime_test n start_time =
   if (prime n)
      then report_prime (get_time_in_milliseconds() - start_time)
      else ()

timed_prime_test n =
   let
     _ = putStrLn(show n)
   in
      start_prime_test n (get_time_in_milliseconds())

-- Exercise 1.25
expmod' nbase nexp m =
   fast_expt (nbase nexp) `mod` m

-- Exercise 1.26
expmod_1 nbase nexp m
   | nexp == 0 = 1
   | otherwise =
      if (even nexp)
         then ((expmod_1 nbase (nexp `div` 2) m) *
               (expmod_1 nbase (nexp `div` 2) m)) `mod` m
         else (nbase * (expmod_1 nbase (nexp - 1) m)) `mod` m


-- 1.3 Formulating Abstractions with Higher-Order Procedures
cube' x = x * x * x


-- 1.3.1 Formulating Abstractions with Higher-Order Procedures - Procedures as Arguments
sum_integers a b =
   if (a > b)
      then 0
      else a + (sum_integers (a + 1) b)

sum_cubes a b =
   if (a > b)
      then 0
      else (cube' a) + (sum_cubes (a + 1) b)

pi_sum a b =
   if (a > b)
      then 0.0
      else (1.0 / (a * (a + 2.0))) + (pi_sum (a + 4.0) b)

sum' term a next b =
   if (a > b)
      then 0
      else (term a) + (sum' term (next a) next b)

-- Using sum
inc' n = n + 1

sum_cubes' a b =
   sum' cube' a inc b

_ = sum_cubes' 1 10

identity x = x

sum_integers' a b =
   sum' identity a inc b

_ = sum_integers' 1 10

sum_1 term a next b =
   if (a > b)
      then 0.0
      else (term a) + (sum_1 term (next a) next b)

pi_sum' a b =
   let
      pi_term x = 1.0 / (x * (x + 2.0))
      pi_next x = x + 4.0
   in
      sum_1 pi_term a pi_next b

_ = 8.0 * (pi_sum 1.0 1000.0)

integral f a b dx =
   let
      add_dx x = x + dx
   in
      (sum_1 f (a + (dx / 2.0)) add_dx b) * dx

cube_1 x = x * x * x

_ = integral cube_1 0.0 1.0 0.01
_ = integral cube_1 0.0 1.0 0.001

-- Exercise 1.32
-- exercise left to reader to define appropriate functions
--    _ = accumulate combiner null_value term a next b


-- 1.3.2 Formulating Abstractions with Higher-Order Procedures - Constructing Procedures Using Lambda
pi_sum_1 a b =
   sum_1 (\x -> 1.0 / (x * (x + 2.0))) a (\x -> x + 4.0) b

integral' f a b dx =
   (sum_1 f (a + (dx / 2.0)) (\x -> x + dx) b) * dx

plus4 x = x + 4

plus4' = \x -> x + 4

_ = (\x -> \y -> \z -> x + y + (square z)) 1 2 3

-- Using let
f_1 x y =
   let
      f_helper a b =
         (x * (square a)) + (y * b) + (a * b)
   in
      f_helper (1 + (x * y)) (1 - y)

f_2 x y =
   (\a -> \b -> (x * (square a)) + (y * b) + (a * b))
      (1 + (x * y)) (1 - y)

f_3 x y =
   let
      a = 1 + (x * y)
      b = 1 - y
   in
      (x * (square a)) + (y * b) + (a * b)

x' = 5
_ =
   let
      x' = 3
   in
      x' + (x' * 10)
   + x

x_1 = 2
_ =
   let
      y = x_1 + 2
   in
      let
         x_1 = 3
      in
         x * y

f_4 x y =
   let
      a = 1 + (x * y)
      b = 1 - y
   in
      (x + (square a)) + (y * b) + (a * b)

-- Exercise 1.34
f_5 g = g 2
_ = f_5 square
_ = f_5 (\z -> z * (z + 1))

-- 1.3.3 Formulating Abstractions with Higher-Order Procedures - Procedures as General Methods

-- Half-interval method
close_enough x y =
   (abs(x - y) < 0.001)

positive x = (x >= 0.0)
negative x = not(positive x)

search f neg_point pos_point =
   let
      midpoint = average neg_point pos_point
   in
      if (close_enough neg_point pos_point)
         then midpoint
         else
            let
               test_value = f midpoint
            in
               if (positive test_value)
                  then search f neg_point midpoint
                  else
                     if (negative test_value)
                        then search f midpoint pos_point
                        else midpoint

-- ??? need to figure out how to throw exception
half_interval_method f a b =
   let
      a_value = f a
      b_value = f b
   in
      if ((negative a_value) && (positive b_value))
         then search f a b
         else
            if ((negative b_value) && (positive a_value))
               then search f b a
               else search f b a -- ??? fail ("Values are not of opposite sign" ++ (show a) ++ " " ++ (show b))

_ = half_interval_method sin 2.0 4.0

_ = half_interval_method (\x -> (x * x * x) - (2.0 * x) - 3.0) 1.0 2.0

-- Fixed points
tolerance = 0.00001

fixed_point f first_guess =
   let
      close_enough v1 v2 =
         abs(v1 - v2) < tolerance
      try guess =
         let
            next = f guess
         in
            if (close_enough guess next)
               then next
               else try next
   in
      try first_guess

_ = fixed_point cos 1.0

_ = fixed_point (\y -> (sin y) + (cos y)) 1.0

-- note: this function does not converge
sqrt_4 x =
   fixed_point(\y -> x / y) 1.0

sqrt_5 x =
   fixed_point (\y -> average y (x / y)) 1.0

-- Exercise 1.37
-- exercise left to reader to define cont_frac
--    cont_frac (\i => 1.0) (\i -> 1.0) k

-- 1.3.4 Formulating Abstractions with Higher-Order Procedures - Procedures as Returned Values
average_damp f =
   \x -> average x (f x)

_ = (average_damp square) 10.0

sqrt_6 x =
   fixed_point (average_damp (\y -> x / y)) 1.0

cube_root x =
   fixed_point (average_damp (\y -> x / (square y))) 1.0

-- Newton's method
dx = 0.00001
deriv g =
   \x -> (g (x + dx) - g (x)) / dx

cube_2 x = x * x * x

_ = (deriv cube_2) 5.0

newton_transform g =
   \x -> x - ((g x) / ((deriv g) x))

newtons_method g guess =
   fixed_point (newton_transform g) guess

sqrt_7 x =
   newtons_method (\y -> (square y) - x) 1.0

-- Fixed point of transformed function
fixed_point_of_transform g transform guess =
   fixed_point (transform g) guess

sqrt_8 x =
   fixed_point_of_transform (\y -> x / y) average_damp 1.0

sqrt_9 x =
   fixed_point_of_transform (\y -> (square y) - x) newton_transform 1.0

-- Exercise 1.40
-- exercise left to reader to define cubic
--    _ = newtons_method (cubic a b c) 1.0

-- Exercise 1.41
-- exercise left to reader to define double
--    _ = ((double (double double)) inc) 5

-- Exercise 1.42
-- exercise left to reader to define compose
--    _ = (compose square inc) 6

-- Exercise 1.43
-- exercise left to reader to define repeated
--    _ = (repeated square 2) 5
