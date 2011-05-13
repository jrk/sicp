# Functions defined in previous chapters #
def gcd(a, b)
   if (b == 0)
      then a
      else gcd(b, a % b)
   end
end

def fib(n)
   if (n == 0)
      then 0
      elsif (n == 1) then 1
      else fib(n - 1) + fib(n - 2)
   end
end

def identity(x) x end


# list functions #
def cons(x, y) [x, y] end
def car(xs) xs[0] end
def cdr(xs) xs[1] end
def cadr(xs) car(cdr(xs)) end

def list(xs)
   if (xs.length == 0)
      then
         nil
      elsif (xs.length == 1) then
         cons(xs[0], nil)
      else
         cons(xs[0], list(xs[1..(xs.length-1)]))
   end
end

def islist(xs)
   begin
      if (xs == nil)
         then true
         elsif (xs[1] == nil) then true
         else islist(cdr(xs))
      end
   rescue
      false
   end
end


# utility functions #
def putx(s)
   def puty(s)
      if (s.kind_of?(Array))
         then
            print "["
            s.each{|i| puty i; print ","}
            print "]"
         else print s
      end
   end
   puty(s)
   print "\n"
end


# 2 Building Abstractions with Data
def linear_combination(a, b, x, y)
   (a * x) + (b * y)
end

def mul(a, b) a * b end
def linear_combination(a, b, x, y)
   mul(a, x) + mul(b, y)
end


# 2.1.1 Introduction to Data Abstraction - Example: Arithmetic Operations for Rational Numbers

# Literal Translation #
def make_rat(n, d) cons(n, d) end
def numer(x) car(x) end
def denom(x) cdr(x) end

def add_rat(x, y)
   make_rat((numer(x) * denom(y)) + (numer(y) * denom(x)), denom(x) * denom(y))
end

def sub_rat(x, y)
   make_rat((numer(x) * denom(y)) - (numer(y) * denom(x)), denom(x) * denom(y))
end

def mul_rat(x, y)
   make_rat(numer(x) * numer(y), denom(x) * denom(y))
end

def div_rat(x, y)
   make_rat(numer(x) * denom(y), denom(x) * numer(y))
end

def equal_rat(x, y)
   ((numer(x) * denom(y)) == (numer(y) * denom(x)))
end

x = cons(1, 2)
y = cons(3, 4)
z = cons(x, y)
putx car(car(z))
putx car(cdr(z))

# footnote -- alternative definitions
# note: ruby distinction b/t def functions and lambdas means can't substitute
# make_rat = lambda{|x,y| cons(x,y)}
# numer = lambda{|x| car(x)}
putx numer(cons(1,2))
# def compose(f, g) lambda{|x| f.call(g(x)) } end
# denom = compose(lambda{|x| car(x)}, lambda{|x| cdr(x)})

def print_rat(x)
   putx (String(numer(x)) + "/" + String(denom(x)))
end

one_half = make_rat(1, 2)
print_rat(one_half)

one_third = make_rat(1, 3)
print_rat(add_rat(one_half, one_third))
print_rat(mul_rat(one_half, one_third))
print_rat(add_rat(one_third, one_third))

# reducing to lowest terms in constructor
def make_rat(n, d)
   g = gcd(n, d)
   cons(n / g, d / g)
end

def add_rat(x, y)
   make_rat((numer(x) * denom(y)) + (numer(y) * denom(x)), denom(x) * denom(y))
end

print_rat(add_rat(one_third, one_third))
# end Literal Translation #

# Object Translation #
class Rational_
   def make_rat(n, d) cons(n, d) end
   def numer(x) car(x) end
   def denom(x) cdr(x) end
   def add_rat(x, y)
      make_rat((numer(x) * denom(y)) + (numer(y) * denom(x)), denom(x) * denom(y))
   end
   def sub_rat(x, y)
      make_rat((numer(x) * denom(y)) - (numer(y) * denom(x)), denom(x) * denom(y))
   end
   def mul_rat(x, y)
      make_rat(numer(x) * numer(y), denom(x) * denom(y))
   end
   def div_rat(x, y)
      make_rat(numer(x) * denom(y), denom(x) * numer(y))
   end
   def equal_rat(x, y)
      ((numer(x) * denom(y)) == (numer(y) * denom(x)))
   end
   def print_rat(x)
      putx (String(numer(x)) + "/" + String(denom(x)))
   end
end
rational = Rational_.new()

one_half = rational.make_rat(1, 2)
rational.print_rat(one_half)

one_third = rational.make_rat(1, 3)
rational.print_rat(rational.add_rat(one_half, one_third))
rational.print_rat(rational.mul_rat(one_half, one_third))
rational.print_rat(rational.add_rat(one_third, one_third))

# reducing to lowest terms in constructor
class Rational_
   def make_rat(n, d)
      g = gcd(n, d)
      cons(n / g, d / g)
   end
   def numer(x) car(x) end
   def denom(x) cdr(x) end
   def add_rat(x, y)
      make_rat((numer(x) * denom(y)) + (numer(y) * denom(x)), denom(x) * denom(y))
   end
   def sub_rat(x, y)
      make_rat((numer(x) * denom(y)) - (numer(y) * denom(x)), denom(x) * denom(y))
   end
   def mul_rat(x, y)
      make_rat(numer(x) * numer(y), denom(x) * denom(y))
   end
   def div_rat(x, y)
      make_rat(numer(x) * denom(y), denom(x) * numer(y))
   end
   def equal_rat(x, y)
      ((numer(x) * denom(y)) == (numer(y) * denom(x)))
   end
   def print_rat(x)
      putx (String(numer(x)) + "/" + String(denom(x)))
   end
end
rational = Rational_.new()

one_third = rational.make_rat(1, 3)
rational.print_rat(rational.add_rat(one_third, one_third))
# end Object Translation #


# 2.1.2 Introduction to Data Abstraction - Abstraction barriers

# Literal Translation #
# reducing to lowest terms in selectors
def make_rat(n, d) cons(n, d) end

def numer(x)
   g = gcd(car(x), cdr(x))
   car(x) / g
end

def denom(x)
   g = gcd(car(x), cdr(x))
   cdr(x) / g
end
# end Literal Translation #

# Module Translation #
# reducing to lowest terms in selectors
class Rational_
   def make_rat(n, d) cons(n, d) end
   def numer(x)
      n = car(x)
      d = cdr(x)
      n / gcd(n, d)
   end
   def denom(x)
      n = car(x)
      d = cdr(x)
      d / gcd(n, d)
   end
   def add_rat(x, y)
      make_rat((numer(x) * denom(y)) + (numer(y) * denom(x)), denom(x) * denom(y))
   end
   def sub_rat(x, y)
      make_rat((numer(x) * denom(y)) - (numer(y) * denom(x)), denom(x) * denom(y))
   end
   def mul_rat(x, y)
      make_rat(numer(x) * numer(y), denom(x) * denom(y))
   end
   def div_rat(x, y)
      make_rat(numer(x) * denom(y), denom(x) * numer(y))
   end
   def equal_rat(x, y)
      ((numer(x) * denom(y)) == (numer(y) * denom(x)))
   end
   def print_rat(x)
      put (String(numer(x)) + "/" + String(denom(x)))
   end
end
rational = Rational_.new()
# end Module Translation #

# Exercise 2.2 #
# exercise left to reader to define appropriate functions
# def print_point(p)
#    putx "(" + String(x_point(p)) + "," + String(y_point(p)) + ")"
# end

# 2.1.3 Introduction to Data Abstraction - What is meant by data?

def cons_(x, y)
   def dispatch(m)
      if (m == 0)
         then x
         elsif (m == 1) then y
         else raise Exception.new("Argument not 0 or 1 -- CONS " + String(m))
      end
   end
   lambda{|m| dispatch(m)}
end

def car_(z) z(0) end
def cdr_(z) z(1) end

# Exercise 2.4 #
def cons_(x, y)
   lambda{|m| m(x, y)}
end
def car_(z)
   z.call(lambda{|p, q| p})
end

# Exercise 2.6 #
zero = lambda{|f| lambda{|x| x}}
def add1(n)
   lambda{|f| lambda{|x| f.call((n.call(f)).call(x))}}
end

# 2.1.4 Introduction to Data Abstraction - Extended Exercise: Interval Arithmetic

# Literal Translation #
# note, ruby lacks tuple type
def make_interval(a, b) cons(a, b) end
def lower_bound(pair) car(x) end
def upper_bound(pair) cdr(y) end

def add_interval(pair)
   x = lower_bound(pair)
   y = upper_bound(pair)
   make_interval(lower_bound(x) + lower_bound(y), upper_bound(x) + upper_bound(y))
end

def mul_interval(pair)
   x = lower_bound(pair)
   y = upper_bound(pair)
   p1 = lower_bound(x) * lower_bound(y)
   p2 = lower_bound(x) * upper_bound(y)
   p3 = upper_bound(x) * lower_bound(y)
   p4 = upper_bound(x) * upper_bound(y)
   make_interval(
      min(min(p1, p2), min(p3, p4)),
      max(max(p1, p2), max(p3, p4)))
end

def div_interval(pair)
   x = lower_bound(pair)
   y = upper_bound(pair)
   z = make_interval(1.0 / upper_bound(y), 1.0 / lower_bound(y))
   mul_interval(x, z)
end

def make_center_width(c, w)
   make_interval(c-w, c+w)
end

def center(i)
   (lower_bound(i) + upper_bound(i)) / 2.0
end

def width(i)
   (upper_bound(i) - lower_bound(i)) / 2.0
end

# parallel resistors
def par1(r1, r2)
   div_interval(mul_interval(r1, r2), add_interval(r1, r2))
end

def par2(r1, r2)
   one = make_interval(1.0, 1.0)
   div_interval(one,
            add_interval(div_interval(one, r1),
                         div_interval(one, r2)))
end
# end Literal Translation #

# Object Translation #
class Interval
   def make_interval(a, b) cons(a, b) end
   def lower_bound(pair) car(x) end
   def upper_bound(pair) cdr(y) end
   def add_interval(pair)
      x = lower_bound(pair)
      y = upper_bound(pair)
      make_interval(lower_bound(x) + lower_bound(y), upper_bound(x) + upper_bound(y))
   end
   def mul_interval(pair)
      x = lower_bound(pair)
      y = upper_bound(pair)
      p1 = lower_bound(x) * lower_bound(y)
      p2 = lower_bound(x) * upper_bound(y)
      p3 = upper_bound(x) * lower_bound(y)
      p4 = upper_bound(x) * upper_bound(y)
      make_interval(
         min(min(p1, p2), min(p3, p4)),
         max(max(p1, p2), max(p3, p4)))
   end
   def div_interval(pair)
      x = lower_bound(pair)
      y = upper_bound(pair)
      z = make_interval(1.0 / upper_bound(y), 1.0 / lower_bound(y))
      mul_interval(x, z)
   end
   def make_center_width(c, w)
      make_interval(c-w, c+w)
   end
   def center(i)
      (lower_bound(i) + upper_bound(i)) / 2.0
   end
   def width(i)
      (upper_bound(i) - lower_bound(i)) / 2.0
   end
end
interval = Interval.new()

# parallel resistors
def par1(r1, r2)
   interval.div_interval(interval.mul_interval(r1, r2), interval.add_interval(r1, r2))
end

def par2(r1, r2)
   one = interval.make_interval(1.0, 1.0)
   interval.div_interval(one,
            interval.add_interval(interval.div_interval(one, r1),
                                  interval.div_interval(one, r2)))
end
# end Object Translation #


# 2.2.1 Hierarchical Data and the Closure Property - Representing Sequences

cons(1, cons(2, cons(3, cons(4, nil))))
one_through_four = list([1, 2, 3, 4])

putx one_through_four
putx car(one_through_four)
putx cdr(one_through_four)
putx car(cdr(one_through_four))
putx cons(10, one_through_four)

def list_ref(items, n)
   if (n == 0)
      then car(items)
      else list_ref(cdr(items), n-1)
   end
end

squares = list([1, 4, 9, 16, 25])
putx list_ref(squares, 3)

def length(items)
   if (items == nil)
      then 0
      else 1 + length(cdr(items))
   end
end

odds = list([1, 3, 5, 7])
putx length(odds)

def length(items)
   def length_iter(a, count)
      if (a == nil)
         then count
         else length_iter(cdr(a), 1+count)
      end
   end
   length_iter(items, 0)
end

# really need to figure out the cons operator ::
def append(list1, list2)
   if (list1 == nil)
      then list2
      else cons(car(list1), append(cdr(list1), list2))
   end
end

putx append(squares, odds)
putx append(odds, squares)

# Mapping over lists
def scale_list(factor, items)
   if (items == nil)
      then nil
      else cons(car(items) * factor, scale_list(factor, cdr(items)))
   end
end

putx scale_list(10, list([1, 2, 3, 4, 5]))

# uncurried version of map
def map(proc, items)
   if (items == nil)
      then nil
      else cons(proc.call(car(items)), map(proc, cdr(items)))
   end
end

putx map(lambda{|x| x.abs}, list([-10, 2.5, -11.6, 17]))
putx map(lambda{|x| x * x}, list([1, 2, 3, 4]))
def scale_list(factor, items)
   map(lambda{|x| x * factor}, items)
end

# curried version map
def map(proc)
   map_lambda = lambda{|items|
      if (items == nil)
         then nil
         else cons(proc.call(car(items)), map(proc).call(cdr(items)))
      end
   }
   lambda{|items| map_lambda.call(items)}
end
putx map(lambda{|x| x.abs}).call(list([-10, 2.5, -11.6, 17]))
putx map(lambda{|x| x * x}).call(list([1, 2, 3, 4]))
def scale_list(items, factor)
   map(lambda{|x| x * factor}).call(items)
end

putx list([1,2,list([3,4])])

# Not sure how to translate these to ruby?
#    (map + (list 1 2 3) (list 40 50 60) (list 700 800 900))
#    (map (lambda (x y) (+ x ( * 2 y))) (list 1 2 3) (list 4 5 6))

# Exercise 2.17 #
# exercise left to reader to define appropriate functions
# putx last_pair(list([23, 72, 149, 34]))

# Exercise 2.18 #
# exercise left to reader to define appropriate functions
# putx reverse(list([1, 4, 9, 16, 25]))

# Exercise 2.19 #
# exercise left to reader to define appropriate functions
# us_coins = list([50, 25, 10, 5, 1])
# uk_coins = list([100, 50, 20, 10, 5, 2, 1, 0.5])
# def cc(amount, coin_values)
#    if (amount == 0)
#       then 1
#       elsif ((amount < 0) || (no_more(coin_values))) then 0
#       else (cc(amount, except_first_denomination(coin_values)) +
#             cc(amount - first_denomination(coin_values), coin_values))
#    end
# end
# putx cc(100, us_coins)

# Exercise 2.20 #
# exercise left to reader to define appropriate functions
# putx same_parity(list([1, 2, 3, 4, 5, 6, 7]))
# putx same_parity(list([2, 3, 4, 5, 6, 7]))

# Exercise 2.21 #
# exercise left to reader to define appropriate functions
# putx square_list(list([1, 2, 3, 4]))

# Exercise 2.22 #
def square(x) x * x end
def square_list(items)
   def iter(things, answer)
      if (things == nil)
         then answer
         else iter(cdr(things), cons(square(car(things)), answer))
      end
   end
   iter(items, nil)
end

def square_list(items)
   def iter(things, answer)
      if (things == nil)
         then answer
         else iter(cdr(things), append(answer, list([square(car(things))])))
      end
   end
   iter(items, nil)
end

# Exercise 2.23 #
def for_each(f, xs)
   if (xs == nil)
      then nil
      else
         f.call(car(xs))
         for_each(f, cdr(xs))
   end
end
def printexp(s) putx s end
for_each(lambda{|x| printexp(x)}, list([57, 321, 88]))

# 2.2.2 Hierarchical Data and the Closure Property - Hierarchical Structures
def count_leaves(tree)
   if (tree == nil)
      then 0
      elsif not(islist(tree)) then 1
      else count_leaves(car(tree)) + count_leaves(cdr(tree))
   end
end

x = list([list([1, 2]), list([3, 4])])
putx length(x)
putx count_leaves(x)

putx list([x, x])
putx length(list([x, x]))
putx count_leaves(list([x, x]))

# Mapping over trees
def scale_tree(factor, tree)
   if (tree == nil)
      then nil
      elsif not(islist(tree))
         tree * factor
      else
         cons(scale_tree(factor, car(tree)), scale_tree(factor, cdr(tree)))
   end
end

putx scale_tree(10, list([1, list([2, list([3, 4]), 5]), list([6, 7])]))

def scale_tree(factor, tree)
   map(
      lambda{|sub_tree|
         if (islist(sub_tree))
            then scale_tree(factor, sub_tree)
            else sub_tree * factor
         end
      }).call(tree)
end

putx scale_tree(10, list([1, list([2, list([3, 4]), 5]), list([6, 7])]))

# Exercise 2.24 #
list([1, list([2, list([3, 4])])])

# Exercise 2.25 #
list([1, 3, list([5, 7]), 9])
list([list([7])])
list([1, list([2, list([3, list([4, list([5, list([6, 7])])])])])])

# Exercise 2.26 #
x = list([1, 2, 3])
y = list([4, 5, 6])
putx x + y
putx list([x, y])
putx list([x, y])

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
def make_mobile(left, right) list([left, right]) end
def make_branch(length, struc) list([length, struc]) end

# Exercise 2.30 #
# exercise left to reader to define appropriate functions
# square_tree(list([1, list([2, list([3, 4]), 5]), list([6, 7])]))

# Exercise 2.31 #
# exercise left to reader to define appropriate functions
# def square_tree(tree) tree_map(square).call(tree) end

# Exercise 2.32 #
# exercise left to reader to define appropriate functions
# def subsets(s)
#    if (s == nil)
#       then nil
#       else
#          rest = subsets(cdr(s))
#          append(rest, map(??FILL_THIS_IN??).call(rest))
#    end
# end


# 2.2.3 Hierarchical Data and the Closure Property - Sequences as Conventional Interfaces
def isodd(n) ((n % 2) == 1) end
def iseven(n) ((n % 2) != 1) end
def square(x) x * x end

def sum_odd_squares(tree)
   if (tree == nil)
      then 0
      elsif not(islist(tree)) then
         if (isodd(tree))
            then square(tree)
            else 0
         end
      else sum_odd_squares(car(tree)) +
           sum_odd_squares(cdr(tree))
   end
end

def even_fibs(n)
   nxt = lambda{|k|
      if (k > n)
         then nil
         else
            f = fib(k)
            if (iseven(f))
               then cons(f, nxt.call(k+1))
               else nxt.call(k+1)
            end
      end
   }
   nxt.call(0)
end

# Sequence operations #
putx map(lambda{|x| square(x)}).call(list([1,2,3,4,5]))

# non-curried version of filter
def filter(predicate, sequence)
   if (sequence == nil)
      then nil
      else
         if (predicate.call(car(sequence)))
            then cons(car(sequence), filter(predicate, cdr(sequence)))
            else filter(predicate, cdr(sequence))
         end
   end
end

putx filter(lambda{|x| isodd(x)}, list([1,2,3,4,5]))

# curried version of filter
def filter(predicate)
   filter_lambda = lambda{|sequence|
      if (sequence == nil)
         then nil
         else
            if (predicate.call(car(sequence)))
               then cons(car(sequence), filter(predicate).call(cdr(sequence)))
               else filter(predicate).call(cdr(sequence))
            end
      end
   }
   filter_lambda
end

putx filter(lambda{|x| isodd(x)}).call(list([1,2,3,4,5]))

# non-curried version of accumulate (aka foldl)
def accumulate(oper, initial, sequence)
   if (sequence == nil)
      then initial
      else oper.call(car(sequence), accumulate(oper, initial, cdr(sequence)))
   end
end

putx accumulate(lambda{|x,y| x+y}, 0, list([1,2,3,4,5]))
putx accumulate(lambda{|x,y| x*y}, 1, list([1,2,3,4,5]))
putx accumulate(lambda{|x,y| cons(x,y)}, nil, list([1,2,3,4,5]))

# curried version of accumulate (aka foldl)
def accumulate(oper)
   initial_lambda = lambda{|initial|
      sequence_lambda = lambda{|sequence|
         if (sequence == nil)
            then initial
            else oper.call(car(sequence), accumulate(oper).call(initial).call(cdr(sequence)))
         end
      }
      sequence_lambda
   }
   initial_lambda
end

putx accumulate(lambda{|x,y| x+y}).call(0).call(list([1,2,3,4,5]))
putx accumulate(lambda{|x,y| x*y}).call(1).call(list([1,2,3,4,5]))
putx accumulate(lambda{|x,y| cons(x,y)}).call(nil).call(list([1,2,3,4,5]))

def enumerate_interval(low, high)
   if (low > high)
      then nil
      else cons(low, enumerate_interval(low+1, high))
   end
end

putx enumerate_interval(2,7)

def enumerate_tree(tree)
   if (tree == nil)
      then nil
      elsif not(islist(tree)) then list([tree])
      else append(enumerate_tree(car(tree)),
                  enumerate_tree(cdr(tree)))
   end
end

putx enumerate_tree(list([1, list([2, list([3, 4]), 5])]))

def sum_odd_squares(tree)
   accumulate(lambda{|x,y| x+y}).call(0).call(
         map(lambda{|a| square(a)}).call(
            filter(lambda{|a| isodd(a)}).call(enumerate_tree(tree))))
end

def even_fibs(n)
   accumulate(lambda{|x,y| cons(x,y)}).call(nil).call(
      filter(lambda{|a| iseven(a)}).call(
         map(lambda{|a| fib(a)}).call(enumerate_interval(0, n))))
end

def list_fib_squares(n)
   accumulate(lambda{|x,y| cons(x,y)}).call(nil).call(
         map(lambda{|a| square(a)}).call(
            map(lambda{|a| fib(a)}).call(enumerate_interval(0, n))))
end

putx list_fib_squares(10)

def product_of_squares_of_odd_elements(sequence)
   accumulate(lambda{|x,y| x*y}).call(1).call(
      map(lambda{|a| square(a)}).call(
         filter(lambda{|a| isodd(a)}).call(sequence)))
end

putx product_of_squares_of_odd_elements(list([1,2,3,4,5]))

class Employee
   attr_accessor :empname
   attr_accessor :jobtitle
   attr_accessor :salary
   def initialize(empname, jobtitle, salary)
      @empname = empname
      @jobtitle = jobtitle
      @salary = salary
   end
   def isProgrammer()
      (@jobtitle == "Programmer")
   end
   def getSalary()
      @salary
   end
end

def salary_of_highest_paid_programmer(records)
   accumulate(lambda{|x,y| if (x>y) then x else y end}).call(0).call(
      map(lambda{|x| x.getSalary()}).call(
         filter(lambda{|x| x.isProgrammer()}).call(records)))
end

recs = list([Employee.new(empname="Fred", jobtitle="Programmer", salary=180),
             Employee.new(empname="Hank", jobtitle="Programmer", salary=150)])
putx salary_of_highest_paid_programmer(recs)

# Nested mappings
n = 5                   # book doesn't define n
putx accumulate(lambda{|x,y| append(x,y)}).call(nil).call(
         map(lambda{|i| map(lambda{|j| list([i, j])}).call(
            enumerate_interval(1, i-1))}).call(
         enumerate_interval(1, n)))

def flatmap(proc)
   flatmap_lambda = lambda{|seq|
      accumulate(lambda{|x,y| append(x,y)}).call(nil).call(map(proc).call(seq))
   }
   flatmap_lambda
end

def has_no_divisors(n, c)
   if (c == 1)
      then true
      elsif ((n % c) == 0) then false
      else has_no_divisors(n, c-1)
   end
end

def isPrime(n)
   has_no_divisors(n, n-1)
end

def prime_sum(pair)
   isPrime(car(pair) + cadr(pair))
end

def make_pair_sum(pair)
   list([car(pair), cadr(pair), car(pair) + cadr(pair)])
end

def prime_sum_pairs(n)
   map(lambda{|x| make_pair_sum(x)}).call(
      filter(lambda{|x| prime_sum(x)}).call(
         flatmap(
            lambda{|i| map(lambda{|j| list([i,j])}).call(enumerate_interval(1, i-1))}).call(
            enumerate_interval(1, n))))
end

putx prime_sum_pairs(5)

def remove(item, sequence)
   filter(lambda{|x| x != item}).call(sequence)
end

def permutations(s)
   if (s == nil)
      then list([nil])
      else
         flatmap(
            lambda{|x|
               map(lambda{|p| cons(x, p)}).call(
                  permutations(remove(x, s))) }).call(s)
   end
end

putx permutations(list([1,2,3]))

# Exercise 2.34 #
# exercise left to reader to define appropriate functions
# def horner_eval(x, coefficient_sequence):
#    accumulate(lambda{|this_coeff, higher_terms| ??FILL_THIS_IN??}.call(0).call(coefficient_sequence)
# horner_eval(2, list([1,3,0,5,0,1]))

# Exercise 2.36 #
# exercise left to reader to define appropriate functions
# def accumulate_n(oper)
#    initial_lambda = lambda{|initial|
#       sequence_lambda = lambda{|sequence|
#          if (sequence == nil)
#             then initial
#             else
#                cons(accumulate(oper).call(init).call(??FILL_THIS_IN??),
#                     accumulate_n(oper).call(init).call(??FILL_THIS_IN??))
#          end
#       }
#       sequence_lambda
#    }
#    initial_lambda
# end
# accumulate_n(lambda{|x,y| x+y}) (0) (s)

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
def fold_right(x) accumulate(x) end
def fold_left(oper)
   initial_lambda = lambda{|initial|
      sequence_lambda = lambda{|sequence|
         iter = lambda{|result, xs|
            if (xs == nil)
               then result
               else iter.call(oper.call(result, car(xs)), cdr(xs))
            end
         }
         iter.call(initial, sequence)
      }
      sequence_lambda
   }
   initial_lambda
end
putx fold_right(lambda{|x,y| x/y}).call(1.0).call(list([1.0,2.0,3.0]))
putx fold_left(lambda{|x,y| x/y}).call(1.0).call(list([1.0,2.0,3.0]))
putx fold_right(lambda{|x,y| cons(x,y)}).call(nil).call(list([1,2,3]))
# CMR Error - won't compile - Scheme result = (((() 1) 2) 3) #
# putx fold_left(lambda{|x,y| cons(x,y)}).call(nil).call(list([1,2,3]))

# Exercise 2.42 #
# exercise left to reader to define appropriate functions
# def queens(board_size)
#    def queen_cols(k)
#       if (k == 0)
#          then list([empty_board])
#          else
#             filter(
#                lambda{|positions| isSafe(k, positions)}).call(
#                flatmap(
#                   lambda{|rest_of_queens|
#                      map(
#                         lambda{|new_row| adjoin_position(new_row, k, rest_of_queens)}).call(
#                         enumerate_interval(1, board_size))}).call(
#                   queen_cols(k-1)))
#       end
#    end
#    queen_cols(board_size)
# end

# Exercise 2.43 #
# exercise left to reader to define appropriate functions
# def queens(board_size)
#    def queen_cols(k)
#       if (k == 0)
#          then list([empty_board])
#          else
#             filter(
#                lambda{|positions| isSafe(k, positions)}).call(
#                flatmap(
#                   lambda{|new_row|
#                      map(
#                         lambda{|rest_of_queens| adjoin_position(new_row, k, rest_of_queens)}).call(
#                         queen_cols(k-1))}).call(
#                   enumerate_interval(1, board_size)))
#       end
#    end
#    queen_cols(board_size)
# end

# 2.2.4 Hierarchical Data and the Closure Property - Example: a picture language

# these two routines are to be written #
def draw_line(x, y) end
def wave(xframe) xframe end

class Vect
   attr_accessor :x
   attr_accessor :y
   def initialize(x, y)
      @x = x
      @y = y
   end
end

def make_vect(x, y) Vect.new(x, y) end
def xcor_vect(v) v.x end
def ycor_vect(v) v.y end
def add_vect(v1, v2)
   make_vect(xcor_vect(v1) + xcor_vect(v2), ycor_vect(v1) + ycor_vect(v2))
end
def sub_vect(v1, v2)
   make_vect(xcor_vect(v1) - xcor_vect(v2), ycor_vect(v1) - ycor_vect(v2))
end
def scale_vect(s, v)
   make_vect(s * xcor_vect(v), s * ycor_vect(v))
end

class Frame
   attr_accessor :orig
   attr_accessor :edge1
   attr_accessor :edge2
   def initialize(orig, edge1, edge2)
      @orig = orig
      @edge1 = edge1
      @edge2 = edge2
   end
end

def make_frame(origin, edge1, edge2)
   Frame.new(origin, edge1, edge2)
end
def origin_frame(f) f.orig() end
def edge1_frame(f) f.edge1() end
def edge2_frame(f) f.edge2() end
a_frame = make_frame(make_vect(0.0, 0.0), make_vect(1.0, 0.0), make_vect(0.0, 1.0))

class Segment
   attr_accessor :x
   attr_accessor :y
   def initialize(x, y)
      @x = x
      @y = y
   end
end

def start_segment(seg) seg.x end
def end_segment(seg) seg.x end

# Frames #
def frame_coord_map(xframe, v)
   add_vect(
      origin_frame(xframe),
      add_vect(scale_vect(xcor_vect(v), edge1_frame(xframe)),
               scale_vect(ycor_vect(v), edge2_frame(xframe))))
end

frame_coord_map(a_frame, make_vect(0.0, 0.0))
origin_frame(a_frame)

# Painters #
def foreach(f)
   foreach_lambda = lambda{|xs|
      if (xs == nil)
         then ()
         else
            f.call(car(xs))
            foreach(f).call(cdr(xs))
      end
   }
   foreach_lambda
end

def segments_painter(segment_list, xframe)
   foreach(
      lambda{|segment|
         draw_line(frame_coord_map(xframe).call(start_segment, segment),
                   frame_coord_map(xframe).call(end_segment, segment))}).call(
      segment_list)
end

def transform_painter(painter, origin, corner1, corner2)
   transform_painter_lambda = lambda{|xframe|
      m = frame_coord_map(xframe)
      new_origin = m(origin)
      painter(
         make_frame(
            new_origin,
            sub_vect(m(corner1), new_origin),
            sub_vect(m(corner2), new_origin)))
   }
   transform_painter_lambda
end

def flip_vert(painter)
   transform_painter(
      painter,
      make_vect(0.0, 1.0),
      make_vect(1.0, 1.0),
      make_vect(0.0, 0.0))
end

def flip_horiz(painter)
   transform_painter(
      painter,
      make_vect(1.0, 0.0),
      make_vect(0.0, 0.0),
      make_vect(1.0, 1.0))
end

def shrink_to_upper_right(painter)
   transform_painter(
      painter,
      make_vect(0.5, 0.5),
      make_vect(1.0, 0.5),
      make_vect(0.5, 1.0))
end

def rotate90(painter)
   transform_painter(
      painter,
      make_vect(1.0, 0.0),
      make_vect(1.0, 1.0),
      make_vect(0.0, 0.0))
end

def rotate180(painter)
   transform_painter(
      painter,
      make_vect(1.0, 1.0),
      make_vect(0.0, 1.0),
      make_vect(1.0, 0.0))
end

def squash_inwards(painter)
   transform_painter(
      painter,
      make_vect(0.0, 0.0),
      make_vect(0.65, 0.35),
      make_vect(0.35, 0.65))
end

def beside(painter1, painter2)
   beside_lambda = lambda{|xframe|
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
      paint_left.call(xframe)
      paint_right.call(xframe)
   }
   beside_lambda
end

def below(painter1, painter2)
   below_lambda = lambda{|xframe|
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
   }
   below_lambda
end

def up_split(painter, n)
   if (n == 0)
      then painter
      else
         smaller = up_split(painter, n-1)
         below(painter, beside(smaller, smaller))
   end
end

wave2 = beside(lambda{|frame| wave(frame)}, flip_vert(lambda{|frame| wave(frame)}))

wave4 = below(wave2, lambda{|frame| wave(frame)})

def flipped_pairs(painter)
   painter2 = beside(painter, flip_vert(painter))
   below(painter2, painter2)
end

wave4 = flipped_pairs(lambda{|frame| wave(frame)})

def right_split(painter, n)
   if (n == 0)
      then painter
      else
         smaller = right_split(painter, n-1)
         beside(painter, below(smaller, smaller))
   end
end

def corner_split(painter, n)
   if (n == 0)
      then painter
      else
         up = up_split(painter, n-1)
         right = right_split(painter, n-1)
         top_left = beside(up, up)
         bottom_right = below(right, right)
         corner = corner_split(painter, n-1)
         beside(below(painter, top_left),  below(bottom_right, corner))
    end
end

def square_limit(painter, n)
   quarter = corner_split(painter, n)
   half = beside(flip_horiz(quarter), quarter)
   below(flip_vert(half), half)
end

# Higher_order operations #
def square_of_four(tleft, tright, bleft, bright)
   square_of_four_lambda = lambda{|painter|
      top = beside(tleft(painter), tright(painter))
      bottom = beside(bright(painter), bright(painter))
      below(bottom, top)
   }
   square_of_four_lambda
end

def flipped_pairs(painter)
   combine4 = square_of_four(identity, flip_vert, identity, flip_vert)
   combine4.call(painter)
end

# footnote #
flipped_pairs = square_of_four(
                     lambda{|x| identity(x)},
                     lambda{|x| flip_vert(x)},
                     lambda{|x| identity(x)},
                     lambda{|x| flip_vert(x)})

def square_limit(painter, n)
   combine4 = square_of_four(flip_horiz, identity, rotate180, flip_vert)
   combine4.call(corner_split(painter, n))
end

# Exercise 2.45 #
# exercise left to reader to define appropriate functions
# right_split = split(lambda{|x,y| beside(x,y)}, lambda{|x,y| below(x,y)})
# up_split = split(below, beside)

# Exercise 2.47 #
def make_frame(origin, edge1, edge2)
   [origin, edge1, edge2]
end
def make_frame(origin, edge1, edge2)
   [origin, [edge1, edge2]]
end
