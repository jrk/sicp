module SICP02() where

-- Functions defined in previous chapters
gcd' a b
   | b == 0 = a
   | otherwise = gcd' b (a `mod` b)
fib n =
   case n of
      0 -> 0
      1 -> 1
      _ -> fib (n - 1) + fib (n - 2)
identity x = x


-- 2 Building Abstractions with Data
linear_combination a b x y =
   (a * x) + (b * y)

mul a b = a * b
linear_combination' a b x y =
   (mul a x) + (mul b y)


-- 2.1.1 Introduction to Data Abstraction - Example: Arithmetic Operations for Rational Numbers

make_rat n d = [n, d]
numer x = head x
denom x = head (tail x)

add_rat x y =
   make_rat ((numer x) * (denom y) + (numer y) * (denom x))
            ((denom x) * (denom y))
sub_rat x y =
   make_rat ((numer x) * (denom y) - (numer y) * (denom x))
            ((denom x) * (denom y))
mul_rat x y =
   make_rat ((numer x) * (numer y))
            ((denom x) * (denom y))
div_rat x y =
   make_rat ((numer x) * (denom y))
            ((denom x) * (numer y))
equal_rat x y =
   (((numer x) * (denom y)) == ((numer y) * (denom x)))

cons x y = [x, y]
car = head
cdr = tail
cadr = car . cdr
cadr' x = car (cdr x)

x' = cons 1 2
_ = car x'
_ = cdr x'

x = cons 1 2
y = cons 3 4
z = cons x y
_ = car (car z)
_ = car (cdr z)

-- footnote -- alternative definitions
make_rat' = cons
numer' = car
denom' = car . cdr

print_rat x =
   putStrLn ((show (numer x)) ++ "/" ++ (show (denom x)))

one_half = make_rat 1 2
_ = print_rat one_half

one_third = make_rat 1 3
_ = print_rat (add_rat one_half one_third)
_ = print_rat (mul_rat one_half one_third)
_ = print_rat (add_rat one_third one_third)

-- reducing to lowest terms in constructor
make_rat_1 n d = [n `div` g, d `div` g]
   where g = gcd n d

add_rat_1 x y =
   make_rat_1 (((numer x) * (denom y)) + ((numer y) * (denom x)))
              ((denom x) * (denom y))

_  = print_rat (add_rat_1 one_third one_third)


-- 2.1.2 Introduction to Data Abstraction - Abstraction barriers

-- reducing to lowest terms in selectors
make_rat_2 n d = cons n d

numer_2 x = (car x) `div` g
   where g = gcd (car x) (cadr x)

denom_2 x = (cadr x) `div` g
   where g = gcd (car x) (cadr x)

-- Exercise 2.2
-- exercise left to reader to define appropriate functions
-- print_point p =
--    putStrLn  ("(" ++ (show (x_point p)) ++ "," ++ (show (y_point p)) ++ ")")


-- 2.1.3 Introduction to Data Abstraction - What is meant by data?
