#!/usr/local/bin/python
# -*- coding: UTF-8 -*-

# Functions defined in previous chapters #
def gcd(a, b):
   if (b == 0):
      return a
   else:
      return gcd(b, a % b)

def fib(n):
   if (n == 0):
      return 0
   elif (n == 1):
      return 1
   else:
      return fib(n - 1) + fib(n - 2)

def identity(x): return x


# list functions #
nil = "<nil>"
def cons(x, y): return [x, y]
def car(xs): return xs[0]
def cdr(xs): return xs[1]
def cadr(xs): return car(cdr(xs))
def list(xs):
   if (len(xs) == 0):
      return nil
   elif (len(xs) == 1):
      return cons(xs[0], nil)
   else:
      return cons(xs[0], list(xs[1:]))

def islist(xs):
   try:
      if (xs == nil):
         return 1
      elif (xs[1] == nil):
         return 1
      else:
         return islist(cdr(xs))
   except:
      return 0


# 2 Building Abstractions with Data
def linear_combination(a, b, x, y):
   return (a * x) + (b * y)

def mul(a, b): return a * b
def linear_combination(a, b, x, y):
   return mul(a, x) + mul(b, y)


# 2.1.1 Introduction to Data Abstraction - Example: Arithmetic Operations for Rational Numbers

# Literal Translation #
def make_rat(n, d): return cons(n, d)
def numer(x): return car(x)
def denom(x): return cdr(x)

def add_rat(x, y):
   return make_rat((numer(x) * denom(y)) + (numer(y) * denom(x)), denom(x) * denom(y))

def sub_rat(x, y):
   return make_rat((numer(x) * denom(y)) - (numer(y) * denom(x)), denom(x) * denom(y))

def mul_rat(x, y):
   return make_rat(numer(x) * numer(y), denom(x) * denom(y))

def div_rat(x, y):
   return make_rat(numer(x) * denom(y), denom(x) * numer(y))

def equal_rat(x, y):
   return ((numer(x) * denom(y)) == (numer(y) * denom(x)))

x = cons(1, 2)

x = cons(1, 2)
y = cons(3, 4)
z = cons(x, y)
print car(car(z))
print car(cdr(z))

# footnote -- alternative definitions
make_rat = cons
numer = car
denom = cdr

def compose(f, g): return lambda x: f(g(x))

def print_rat(x):
   print str(numer(x)) + "/" + str(denom(x))

one_half = make_rat(1, 2)
print_rat(one_half)

one_third = make_rat(1, 3)
print_rat(add_rat(one_half, one_third))
print_rat(mul_rat(one_half, one_third))
print_rat(add_rat(one_third, one_third))

# reducing to lowest terms in constructor
def make_rat(n, d):
   g = gcd(n, d)
   return cons(n / g, d / g)

def add_rat(x, y):
   return make_rat((numer(x) * denom(y)) + (numer(y) * denom(x)), denom(x) * denom(y))

print_rat(add_rat(one_third, one_third))
# end Literal Translation #

# Object Translation #
class Rational:
   def make_rat(self, n, d): return cons(n, d)
   def numer(self, x): return car(x)
   def denom(self, x): return cdr(x)
   def add_rat(self, x, y):
      return self.make_rat(
         (self.numer(x) * self.denom(y)) + (self.numer(y) * self.denom(x)),
         self.denom(x) * self.denom(y))
   def sub_rat(self, x, y):
      return self.make_rat(
         (self.numer(x) * self.denom(y)) - (self.numer(y) * self.denom(x)),
         self.denom(x) * self.denom(y))
   def mul_rat(self, x, y):
      return self.make_rat(self.numer(x) * self.numer(y), self.denom(x) * self.denom(y))
   def div_rat(self, x, y):
      return self.make_rat(self.numer(x) * self.denom(y), self.denom(x) * self.numer(y))
   def equal_rat(self, x, y):
      return ((self.numer(x) * self.denom(y)) == (self.numer(y) * self.denom(x)))
   def print_rat(self, x):
      print (str(self.numer(x)) + "/" + str(self.denom(x)))
rational = Rational()

one_half = rational.make_rat(1, 2)
rational.print_rat(one_half)

one_third = rational.make_rat(1, 3)
rational.print_rat(rational.add_rat(one_half, one_third))
rational.print_rat(rational.mul_rat(one_half, one_third))
rational.print_rat(rational.add_rat(one_third, one_third))

# reducing to lowest terms in constructor
class Rational:
   def make_rat(self, n, d):
      g = gcd(n, d)
      return cons(n / g, d / g)
   def numer(self, x): return car(x)
   def denom(self, x): return cdr(x)
   def add_rat(self, x, y):
      return self.make_rat(
         (self.numer(x) * self.denom(y)) + (self.numer(y) * self.denom(x)),
         self.denom(x) * self.denom(y))
   def sub_rat(self, x, y):
      return self.make_rat(
         (self.numer(x) * self.denom(y)) - (self.numer(y) * self.denom(x)),
         self.denom(x) * self.denom(y))
   def mul_rat(self, x, y):
      return self.make_rat(self.numer(x) * self.numer(y), self.denom(x) * self.denom(y))
   def div_rat(self, x, y):
      return self.make_rat(self.numer(x) * self.denom(y), self.denom(x) * self.numer(y))
   def equal_rat(self, x, y):
      return ((self.numer(x) * self.denom(y)) == (self.numer(y) * self.denom(x)))
   def print_rat(self, x):
      print (str(self.numer(x)) + "/" + str(self.denom(x)))
rational = Rational()

one_third = rational.make_rat(1, 3)
rational.print_rat(rational.add_rat(one_third, one_third))
# end Object Translation #


# 2.1.2 Introduction to Data Abstraction - Abstraction barriers

# Literal Translation #
# reducing to lowest terms in selectors
def make_rat(n, d): return cons(n, d)

def numer(x):
   g = gcd(car(x), cdr(x))
   return car(x) / g

def denom(x):
   g = gcd(car(x), cdr(x))
   return cdr(x) / g
# end Literal Translation #

# Object Translation #
# reducing to lowest terms in selectors
class Rational:
   def make_rat(self, n, d): return cons(n, d)
   def numer(self, x):
      n = car(x)
      d = cdr(x)
      return n / gcd(n, d)
   def denom(self, x):
      n = car(x)
      d = cdr(x)
      return d / gcd(n, d)
   def add_rat(self, x, y):
      return self.make_rat(
         (self.numer(x) * self.denom(y)) + (self.numer(y) * self.denom(x)),
         self.denom(x) * self.denom(y))
   def sub_rat(self, x, y):
      return self.make_rat(
         (self.numer(x) * self.denom(y)) - (self.numer(y) * self.denom(x)),
         self.denom(x) * self.denom(y))
   def mul_rat(self, x, y):
      return self.make_rat(self.numer(x) * self.numer(y), self.denom(x) * self.denom(y))
   def div_rat(self, x, y):
      return self.make_rat(self.numer(x) * self.denom(y), self.denom(x) * self.numer(y))
   def equal_rat(self, x, y):
      return ((self.numer(x) * self.denom(y)) == (self.numer(y) * self.denom(x)))
   def print_rat(self, x):
      print (str(self.numer(x)) + "/" + str(self.denom(x)))
rational = Rational()
# end Object Translation #

# Exercise 2.2 #
# exercise left to reader to define appropriate functions
#def print_point(p):
#   print "(" + str(x_point(p)) + "," + str(y_point(p)) + ")"

# 2.1.3 Introduction to Data Abstraction - What is meant by data?

def cons_(x, y):
   def dispatch(m):
      if (m == 0):
         return x
      elif (m == 1):
         return y
      else:
         raise Exception("Argument not 0 or 1 -- CONS " + str(m))
   return dispatch

def car_(z): return z(0)
def cdr_(z): return z(1)

# Exercise 2.4 #
def cons_(x, y):
   return (lambda m: m(x, y))
def car_(z):
   return z(lambda p, q: p)

# Exercise 2.6 #
zero = lambda f: lambda x: x
def add1(n): return lambda f: lambda x: (f((n(f))(x)))

# 2.1.4 Introduction to Data Abstraction - Extended Exercise: Interval Arithmetic

# Literal Translation #
# note, python function parameters are not a first class tuple
def make_interval(a, b): return (a, b)

def lower_bound((x, y)): return x
def upper_bound((x, y)): return y

def add_interval(x, y):
   return make_interval(lower_bound(x) + lower_bound(y), upper_bound(x) + upper_bound(y))

def mul_interval(x, y):
   p1 = lower_bound(x) * lower_bound(y)
   p2 = lower_bound(x) * upper_bound(y)
   p3 = upper_bound(x) * lower_bound(y)
   p4 = upper_bound(x) * upper_bound(y)
   return make_interval(
      min(min(p1, p2), min(p3, p4)),
      max(max(p1, p2), max(p3, p4)))

def div_interval(x, y):
   z = make_interval(1.0 / upper_bound(y), 1.0 / lower_bound(y))
   return mul_interval(x, z)

def make_center_width(c, w):
   return make_interval(c-w, c+w)

def center(i):
   return (lower_bound(i) + upper_bound(i)) / 2.0

def width(i):
   return (upper_bound(i) - lower_bound(i)) / 2.0

# parallel resistors
def par1(r1, r2):
   return div_interval(mul_interval(r1, r2), add_interval(r1, r2))

def par2(r1, r2):
   one = make_interval(1.0, 1.0)
   return div_interval(one,
            add_interval(div_interval(one, r1),
                         div_interval(one, r2)))
# end Literal Translation #

# Object Translation #
class Interval:
   def make_interval(self, a, b): return (a, b)
   def lower_bound(self, (x, y)): return x
   def upper_bound(self, (x, y)): return y
   def add_interval(self, x, y):
      return self.make_interval(self.lower_bound(x) + self.lower_bound(y),
                                self.upper_bound(x) + self.upper_bound(y))
   def mul_interval(self, x, y):
      p1 = self.lower_bound(x) * self.lower_bound(y)
      p2 = self.lower_bound(x) * self.upper_bound(y)
      p3 = self.upper_bound(x) * self.lower_bound(y)
      p4 = self.upper_bound(x) * self.upper_bound(y)
      return self.make_interval(
         min(min(p1, p2), min(p3, p4)),
         max(max(p1, p2), max(p3, p4)))
   def div_interval(self, x, y):
      z = self.make_interval(1.0 / self.upper_bound(y), 1.0 / self.lower_bound(y))
      return self.mul_interval(x, z)
   def make_center_width(self, c, w):
      return self.make_interval(c-w, c+w)
   def center(self, i):
      return (self.lower_bound(i) + self.upper_bound(i)) / 2.0
   def width(self, i):
      return (self.upper_bound(i) - self.lower_bound(i)) / 2.0
interval = Interval()

# parallel resistors
def par1(r1, r2):
   return interval.div_interval(interval.mul_interval(r1, r2), interval.add_interval(r1, r2))

def par2(r1, r2):
   one = interval.make_interval(1.0, 1.0)
   return interval.div_interval(one,
            interval.add_interval(interval.div_interval(one, r1),
                                  interval.div_interval(one, r2)))
# end Object Translation #

# 2.2.1 Hierarchical Data and the Closure Property - Representing Sequences

cons(1, cons(2, cons(3, cons(4, nil))))
one_through_four = list([1, 2, 3, 4])

print one_through_four
print car(one_through_four)
print cdr(one_through_four)
print car(cdr(one_through_four))
print cons(10, one_through_four)

def list_ref(items, n):
   if (n == 0):
      return car(items)
   else:
      return list_ref(cdr(items), n-1)

squares = list([1, 4, 9, 16, 25])
print list_ref(squares, 3)

def length(items):
   if (items == nil):
      return 0
   else:
      return 1 + length(cdr(items))

odds = list([1, 3, 5, 7])
print length(odds)

def length(items):
   def length_iter(a, count):
      if (a == nil):
         return count
      else:
         return length_iter(cdr(a), 1+count)
   return length_iter(items, 0)

# really need to figure out the cons operator ::
def append(list1, list2):
   if (list1 == nil):
      return list2
   else:
      return cons(car(list1), append(cdr(list1), list2))

print append(squares, odds)
print append(odds, squares)

# Mapping over lists
def scale_list(factor, items):
   if (items == nil):
      return nil
   else:
      return cons((car(items) * factor), scale_list(factor, cdr(items)))

print scale_list(10, list([1, 2, 3, 4, 5]))

# uncurried version of map
def map(proc, items):
   if (items == nil):
      return nil
   else:
      return cons(proc(car(items)), map(proc, cdr(items)))
print map(abs, list([-10, 2.5, -11.6, 17]))
print map(lambda x: x * x, list([1, 2, 3, 4]))
def scale_list(factor, items):
   return map(lambda x: x * factor, items)

# curried version map
def map(proc):
   def map_lambda(items):
      if (items == nil):
         return nil
      else:
         return cons(proc(car(items)), map (proc) (cdr(items)))
   return map_lambda
print map (abs) (list([-10, 2.5, -11.6, 17]))
print map (lambda x: x * x) (list([1, 2, 3, 4]))
def scale_list(factor, items):
   return map (lambda x: x * factor) (items)

# Not sure how to translate these to Python?
#    (map + (list 1 2 3) (list 40 50 60) (list 700 800 900))
#    (map (lambda (x y) (+ x ( * 2 y))) (list 1 2 3) (list 4 5 6))

# Exercise 2.17 #
# exercise left to reader to define appropriate functions
# print last_pair(list([23, 72, 149, 34]))

# Exercise 2.18 #
# exercise left to reader to define appropriate functions
# print reverse(list([1, 4, 9, 16, 25]))

# Exercise 2.19 #
# exercise left to reader to define appropriate functions
# us_coins = list([50, 25, 10, 5, 1])
# uk_coins = list([100, 50, 20, 10, 5, 2, 1, 0.5])
# def cc(amount, coin_values):
#    if (amount == 0):
#       return 1
#    elif ((amount < 0) or (no_more(coin_values))):
#       return 0
#    else:
#       return (cc(amount, except_first_denomination(coin_values)) +
#               cc(amount - first_denomination(coin_values), coin_values))
# print cc(100, us_coins)

# Exercise 2.20 #
# exercise left to reader to define appropriate functions
# print same_parity(list([1, 2, 3, 4, 5, 6, 7]))
# print same_parity(list([2, 3, 4, 5, 6, 7]))

# Exercise 2.21 #
# exercise left to reader to define appropriate functions
# print square_list(list([1, 2, 3, 4]))

# Exercise 2.22 #
def square(x): return x * x
def square_list(items):
   def iter(things, answer):
      if (things == nil):
         return answer
      else:
         return iter(cdr(things), cons(square(car(things)), answer))
   return iter(items, nil)
def square_list(items):
   def iter(things, answer):
      if (things == nil):
         return answer
      else:
         return iter(cdr(things), append(answer, list([square(car(things))])))
   return iter(items, nil)

# Exercise 2.23 #
def for_each(f, xs):
   if (xs == nil):
      return nil
   else:
      f(car(xs))
      return for_each(f, cdr(xs))
def printexp(s): print str(s)
for_each(lambda x: printexp(x), list([57, 321, 88]))


# 2.2.2 Hierarchical Data and the Closure Property - Hierarchical Structures
def count_leaves(tree):
   if (tree == nil):
      return 0
   elif not(islist(tree)):
      return 1
   else:
      return count_leaves(car(tree)) + count_leaves(cdr(tree))

x = list([   list([1, 2]), list([3, 4])   ])
print length(x)
print count_leaves(x)

print list([x, x])
print length(list([x, x]))
print count_leaves(list([x, x]))

# Mapping over trees
def scale_tree(factor, tree):
   if (tree == nil):
      return nil
   elif not(islist(tree)):
      return tree * factor
   else:
      return cons(scale_tree(factor, car(tree)), scale_tree(factor, cdr(tree)))

print scale_tree(10, list([1, list([2, list([3, 4]), 5]), list([6, 7])]))

def scale_tree(factor, tree):
   return map(
      lambda sub_tree:
         scale_tree(factor, sub_tree) if (islist(sub_tree)) else sub_tree * factor
      )(tree)

print scale_tree(10, list([1, list([2, list([3, 4]), 5]), list([6, 7])]))

# Exercise 2.24 #
list([1, list([2, list([3, 4])])])

# Exercise 2.25 #
list([1, 3, list([5, 7]), 9])
list([list([7])])
list([1, list([2, list([3, list([4, list([5, list([6, 7])])])])])])

# Exercise 2.26 #
x = list([1, 2, 3])
y = list([4, 5, 6])
print x + y
print list([x, y])
print list([x, y])

# Exercise 2.27 #
x = list([list([1, 2]), list([3, 4])])
# exercise left to reader to define appropriate functions
# reverse(x)
# deep_reverse(x)

# Exercise 2.28 #
x = list([list([1, 2]), list([3, 4])])
# exercise left to reader to define appropriate functions
# fringe(x)
# fringe(list([x, x]))

# Exercise 2.29 #
def make_mobile(left, right): return list([left, right])
def make_branch(length, struc): return list([length, struc])

# Exercise 2.30 #
# exercise left to reader to define appropriate functions
# square_tree(list([1, list([2, list([3, 4]), 5]), list([6, 7])]))

# Exercise 2.31 #
# exercise left to reader to define appropriate functions
# def square_tree(tree): return tree_map (square) (tree)

# Exercise 2.32 #
# exercise left to reader to define appropriate functions
# def subsets(s):
#    if (s == nil):
#       return nil
#    else:
#       rest = subsets(cdr(s))
#       return append(rest, map (??FILL_THIS_IN??) (rest))

# 2.2.3 Hierarchical Data and the Closure Property - Sequences as Conventional Interfaces
def isodd(n): return ((n % 2) == 1)
def iseven(n): return ((n % 2) != 1)
def square(x): return x * x

def sum_odd_squares(tree):
   if (tree == nil):
      return 0
   elif not(islist(tree)):
      if (isodd(tree)):
         return square(tree)
      else:
         return 0
   else:
      return (sum_odd_squares(car(tree)) +
              sum_odd_squares(cdr(tree)))

def even_fibs(n):
   def next(k):
      if (k > n):
         return nil
      else:
         f = fib(k)
         if (iseven(f)):
            return cons(f, next(k+1))
         else:
            return next(k+1)
   return next(0)

# Sequence operations #
print map (square) (list([1,2,3,4,5]))

# non-curried version of filter
def filter(predicate, sequence):
   if (sequence == nil):
      return nil
   else:
      if (predicate(car(sequence))):
         return cons(car(sequence), filter(predicate, cdr(sequence)))
      else:
         return filter(predicate, cdr(sequence))

print filter(isodd, list([1,2,3,4,5]))

# curried version of filter
def filter(predicate):
   def filter_lambda(sequence):
      if (sequence == nil):
         return nil
      else:
         if (predicate(car(sequence))):
            return cons(car(sequence), filter (predicate) (cdr(sequence)))
         else:
            return filter (predicate) (cdr(sequence))
   return filter_lambda

print filter (isodd) (list([1,2,3,4,5]))

# non-curried version of accumulate (aka foldl)
def accumulate(oper, initial, sequence):
   if (sequence == nil):
      return initial
   else:
      return oper(car(sequence), accumulate(oper, initial, cdr(sequence)))

print accumulate(lambda x,y: x+y, 0, list([1,2,3,4,5]))
print accumulate(lambda x,y: x*y, 1, list([1,2,3,4,5]))
print accumulate(cons, nil, list([1,2,3,4,5]))

# curried version of accumulate (aka foldl)
def accumulate(oper):
   def initial_lambda(initial):
      def sequence_lambda(sequence):
         if (sequence == nil):
            return initial
         else:
            return oper(car(sequence), accumulate (oper) (initial) (cdr(sequence)))
      return sequence_lambda
   return initial_lambda

print accumulate (lambda x,y: x+y) (0) (list([1,2,3,4,5]))
print accumulate (lambda x,y: x*y) (0) (list([1,2,3,4,5]))
print accumulate (cons) (nil) (list([1,2,3,4,5]))

def enumerate_interval(low, high):
   if (low > high):
      return nil
   else:
      return cons(low, enumerate_interval(low+1, high))

print enumerate_interval(2,7)

def enumerate_tree(tree):
   if (tree == nil):
      return nil
   elif not(islist(tree)):
      return list([tree])
   else:
      return append(enumerate_tree(car(tree)),
                    enumerate_tree(cdr(tree)))

print enumerate_tree(list([1, list([2, list([3, 4]), 5])]))

def sum_odd_squares(tree):
   return accumulate (lambda x,y: x+y) (0) (
      map (square) (filter (isodd) (enumerate_tree(tree))))

def even_fibs(n):
   return accumulate (cons) (nil) (
            filter (iseven) (map (fib) (enumerate_interval(0, n))))

def list_fib_squares(n):
   return accumulate (cons) (nil) (
         map (square) (map (fib) (enumerate_interval(0, n))))

print list_fib_squares(10)

def product_of_squares_of_odd_elements(sequence):
   return accumulate (lambda x,y: x*y) (1) (map (square) (filter (isodd) (sequence)))

print product_of_squares_of_odd_elements(list([1,2,3,4,5]))

class Employee:
   def __init__(self, empname, jobtitle, salary):
      self.empname = empname
      self.jobtitle = jobtitle
      self.salary = salary
   def isProgrammer(self):
      return (self.jobtitle == "Programmer")
   def getSalary(self):
      return self.salary

def salary_of_highest_paid_programmer(records):
   return accumulate (max) (0) (map (Employee.getSalary) (filter (Employee.isProgrammer) (records)))

recs = list([Employee(empname="Fred", jobtitle="Programmer", salary=180),
             Employee(empname="Hank", jobtitle="Programmer", salary=150)])
print salary_of_highest_paid_programmer(recs)

# Nested mappings
n = 5                   # book doesn't define n
print accumulate (append) (nil) (
         map
            (lambda i: map
               (lambda j: list([i, j]))
               (enumerate_interval(1, i-1)))
            (enumerate_interval(1, n)))

def flatmap(proc):
   def flatmap_lambda(seq):
      return accumulate (append) (nil) (map (proc) (seq))
   return flatmap_lambda

def has_no_divisors(n, c):
   if (c == 1):
      return True
   elif ((n % c) == 0):
      return False
   else:
      return has_no_divisors(n, c-1)

def isPrime(n):
   return has_no_divisors(n, n-1)

def prime_sum(pair):
   return isPrime(car(pair) + cadr(pair))

def make_pair_sum(pair):
   return list([car(pair), cadr(pair), car(pair) + cadr(pair)])

def prime_sum_pairs(n):
   return map(make_pair_sum)(
            filter
               (prime_sum)
               (flatmap
                  (lambda i: map(lambda j: list([i,j]))(enumerate_interval(1, i-1)))
                  (enumerate_interval(1, n))))

print prime_sum_pairs(5)

def remove(item, sequence):
   return filter (lambda x: x != item) (sequence)

def permutations(s):
   if (s == nil):
      return list([nil])
   else:
      return (
         flatmap
         (lambda x:
            map
               (lambda p: cons(x, p))
               (permutations(remove(x, s))))
         (s))

# Exercise 2.34 #
# exercise left to reader to define appropriate functions
# def horner_eval(x, coefficient_sequence):
#    return accumulate (lambda this_coeff, higher_terms: ??FILL_THIS_IN??) (0) (coefficient_sequence)
# horner_eval(2, list([1,3,0,5,0,1]))

# Exercise 2.36 #
# exercise left to reader to define appropriate functions
# def accumulate_n(oper):
#    def initial_lambda(initial):
#       def sequence_lambda(sequence):
#          if (sequence == nil):
#             return initial
#          else:
#             return cons(accumulate (oper) (init) (??FILL_THIS_IN??),
#                         accumulate_n (oper) (init) (??FILL_THIS_IN??))
#       return sequence_lambda
#    return initial_lambda
# accumulate_n (lambda x,y: x + y) (0) (s)

# CMR Error - need to finish this one #
#  # Exercise 2.37 *
#  def dot_product(v, w) =
#     accumulate
#        op+
#        0
#        (map
#              (fn i =>
#                 map
#                    (fn j => i * j)
#                    w)
#              v)

# Exercise 2.38 #
fold_right = accumulate
def fold_left(oper):
   def initial_lambda(initial):
      def sequence_lambda(sequence):
         def iter(result, xs):
            if (xs == nil):
               return result
            else:
               return iter(oper(result, car(xs)), cdr(xs))
         return iter(initial, sequence)
      return sequence_lambda
   return initial_lambda
print fold_right (lambda x,y: x/y) (1.0) (list([1.0,2.0,3.0]))
print fold_left (lambda x,y: x/y) (1.0) (list([1.0,2.0,3.0]))
print fold_right (cons) (nil) (list([1,2,3]))
# CMR Error - won't compile - Scheme result = (((() 1) 2) 3) #
# print fold_left (lambda x,y: list([x])+y) (nil) (list([1,2,3]))

# Exercise 2.42 #
# exercise left to reader to define appropriate functions
# def queens(board_size):
#    def queen_cols(k):
#       if (k == 0):
#          return list([empty_board])
#       else:
#          return (
#             filter
#                (lambda positions: isSafe(k, positions))
#                (flatmap
#                   (lambda rest_of_queens:
#                      map
#                         (lambda new_row: adjoin_position(new_row, k, rest_of_queens))
#                         (enumerate_interval(1, board_size)))
#                   (queen_cols(k-1))))
#    return queen_cols(board_size)

# Exercise 2.43 #
# exercise left to reader to define appropriate functions
# def queens(board_size):
#    def queen_cols(k):
#       if (k == 0):
#          return list([empty_board])
#       else:
#          return (
#             filter
#                (lambda positions: isSafe(k, positions))
#                (flatmap
#                   (lambda new_row:
#                      map
#                         (lambda rest_of_queens: adjoin_position(new_row, k, rest_of_queens))
#                         (queen_cols(k-1)))
#                   (enumerate_interval(1, board_size))))
#    return queen_cols(board_size)


# 2.2.4 Hierarchical Data and the Closure Property - Example: a picture language

# these two routines are to be written #
def draw_line(x, y): nop
def wave(xframe): return xframe

class Vect:
   def __init__(self, x, y):
      self.x = x
      self.y = y
   def getX(self):
      return self.x
   def getY(self):
      return self.y

def make_vect(x, y): return Vect(x, y)
def xcor_vect(v): return v.getX()
def ycor_vect(v): return v.getY()
def add_vect(v1, v2):
   return make_vect(xcor_vect(v1) + xcor_vect(v2), ycor_vect(v1) + ycor_vect(v2))
def sub_vect(v1, v2):
   return make_vect(xcor_vect(v1) - xcor_vect(v2), ycor_vect(v1) - ycor_vect(v2))
def scale_vect(s, v):
   return make_vect(s * xcor_vect(v), s * ycor_vect(v))

class Frame:
   def __init__(self, orig, edge1, edge2):
      self.orig = orig
      self.edge1 = edge1
      self.edge2 = edge2
   def getOrig(self):
      return self.orig
   def getEdge1(self):
      return self.edge1
   def getEdge2(self):
      return self.edge2

def make_frame(origin, edge1, edge2):
   return Frame(origin, edge1, edge2)
def origin_frame(f): return f.getOrig()
def edge1_frame(f): return f.getEdge1()
def edge2_frame(f): return f.getEdge2()
a_frame = make_frame(make_vect(0.0, 0.0), make_vect(1.0, 0.0), make_vect(0.0, 1.0))

class Segment:
   def __init__(self, x, y):
      self.x = x
      self.y = y
   def getX(self):
      return self.x
   def getY(self):
      return self.y

def start_segment(seg): return seg.getX()
def end_segment(seg): return seg.getY()

# Frames #
def frame_coord_map(xframe, v):
   return add_vect(
      origin_frame(xframe),
      add_vect(scale_vect(xcor_vect(v), edge1_frame(xframe)),
               scale_vect(ycor_vect(v), edge2_frame(xframe))))

frame_coord_map(a_frame, make_vect(0.0, 0.0))
origin_frame(a_frame)

# Painters #
def foreach(f):
   def foreach_lambda(xs):
      if (x == nil):
         nop
      else:
         f(car(xs))
         foreach (f) (cdr(xs))
   return foreach_lambda

def segments_painter(segment_list, xframe):
   (foreach
      (lambda segment:
         draw_line
            (frame_coord_map(xframe)(start_segment, segment)),
            (frame_coord_map(xframe)(end_segment, segment)))
      (segment_list))

def transform_painter(painter, origin, corner1, corner2):
   def transform_painter_lambda(xframe):
      m = frame_coord_map(xframe)
      new_origin = m(origin)
      return painter(
         make_frame(
            new_origin,
            sub_vect(m(corner1), new_origin),
            sub_vect(m(corner2), new_origin)))
   return transform_painter_lambda

def flip_vert(painter):
   return transform_painter(
      painter,
      make_vect(0.0, 1.0),
      make_vect(1.0, 1.0),
      make_vect(0.0, 0.0))

def flip_horiz(painter):
   return transform_painter(
      painter,
      make_vect(1.0, 0.0),
      make_vect(0.0, 0.0),
      make_vect(1.0, 1.0))

def shrink_to_upper_right(painter):
   return transform_painter(
      painter,
      make_vect(0.5, 0.5),
      make_vect(1.0, 0.5),
      make_vect(0.5, 1.0))

def rotate90(painter):
   return transform_painter(
      painter,
      make_vect(1.0, 0.0),
      make_vect(1.0, 1.0),
      make_vect(0.0, 0.0))

def rotate180(painter):
   return transform_painter(
      painter,
      make_vect(1.0, 1.0),
      make_vect(0.0, 1.0),
      make_vect(1.0, 0.0))

def squash_inwards(painter):
   return transform_painter(
      painter,
      make_vect(0.0, 0.0),
      make_vect(0.65, 0.35),
      make_vect(0.35, 0.65))

def beside(painter1, painter2):
   def beside_lambda(xframe):
      split_point = make_vect(0.5, 0.0)
      paint_left = (
         transform_painter(
            painter1,
            make_vect(0.0, 0.0),
            split_point,
            make_vect(0.0, 1.0)))
      paint_right = (
         transform_painter(
            painter2,
            split_point,
            make_vect(1.0, 0.0),
            make_vect(0.5, 1.0)))
      paint_left(xframe)
      paint_right(xframe)
   return beside_lambda

def below(painter1, painter2):
   def below_lambda(xframe):
      split_point = make_vect(0.0, 0.5)
      paint_below = (
         transform_painter(
            painter1,
            make_vect(0.0, 0.0),
            make_vect(1.0, 0.0),
            split_point))
      paint_above = (
         transform_painter(
            painter2,
            split_point,
            make_vect(1.0, 0.5),
            make_vect(0.0, 1.0)))
      paint_below(xframe)
      paint_above(xframe)
   return below_lambda

def up_split(painter, n):
   if (n == 0):
      return painter
   else:
      smaller = up_split(painter, n-1)
      return below(painter, beside(smaller, smaller))

wave2 = beside(wave, flip_vert(wave))

wave4 = below(wave2, wave)

def flipped_pairs(painter):
   painter2 = beside(painter, flip_vert(painter))
   return below(painter2, painter2)

wave4 = flipped_pairs(wave)

def right_split(painter, n):
   if (n == 0):
      return painter
   else:
      smaller = right_split(painter, n-1)
      return beside(painter, below(smaller, smaller))

def corner_split(painter, n):
   if (n == 0):
      return painter
   else:
      up = up_split(painter, n-1)
      right = right_split(painter, n-1)
      top_left = beside(up, up)
      bottom_right = below(right, right)
      corner = corner_split(painter, n-1)
      return beside(below(painter, top_left),  below(bottom_right, corner))

def square_limit(painter, n):
   quarter = corner_split(painter, n)
   half = beside(flip_horiz(quarter), quarter)
   return below(flip_vert(half), half)

# Higher_order operations #
def square_of_four(tleft, tright, bleft, bright):
   def square_of_four_lambda(painter):
      top = beside(tleft(painter), tright(painter))
      bottom = beside(bright(painter), bright(painter))
      return below(bottom, top)
   return square_of_four_lambda

def flipped_pairs(painter):
   combine4 = square_of_four(identity, flip_vert, identity, flip_vert)
   return combine4(painter)

# footnote #
flipped_pairs = square_of_four(identity, flip_vert, identity, flip_vert)

def square_limit(painter, n):
   combine4 = square_of_four(flip_horiz, identity, rotate180, flip_vert)
   return combine4(corner_split(painter, n))

# Exercise 2.45 #
# exercise left to reader to define appropriate functions
# right_split = split(beside, below)
# up_split = split(below, beside)

# Exercise 2.47 #
def make_frame(origin, edge1, edge2):
   return [origin, edge1, edge2]
def make_frame(origin, edge1, edge2):
   return [origin, [edge1, edge2]]
