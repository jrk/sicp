#!/usr/bin/env rune
pragma.syntax("0.9")

# Functions defined in previous chapters #
def gcd(a, b) {
   if (b == 0) {
      return a
   } else {
      return gcd(b, a % b)
   }
}

def fib(n) {
   if (n == 0) {
      return 0
   } else if (n == 1) {
      return 1
   } else {
      return fib(n - 1) + fib(n - 2)
   }
}

def identity(x) { return x }

# list functions #
def nil { }
def cons(x, y) { return [x, y] }
def car(xs) { return xs[0] }
def cdr(xs) { return xs[1] }
def cadr(xs) { return car(cdr(xs)) }

def list(xs) {
   if (xs =~ []) {
      return nil
   } else {
      return cons(xs[0], list(xs(1)))
   }
}

def islist(xs) {
   return switch(xs) {
      match == nil   { true }
      match [_, cdr] { islist(cdr) }
      match _        { false }
   }
}

# utility functions #
def str := E.toString
def abs(x) { return x.abs() }
def min(x, y) { return x.min(y) }
def max(x, y) { return x.max(y) }


# 2 Building Abstractions with Data
def linear_combination(a, b, x, y) {
   return (a * x) + (b * y)
}

def mul(a, b) { return a * b }
def linear_combination(a, b, x, y) {
   return mul(a, x) + mul(b, y)
}

# 2.1.1 Introduction to Data Abstraction - Example: Arithmetic Operations for Rational Numbers

# Literal Translation #
   def make_rat(n, d) { return cons(n, d) }
   def numer(x) { return car(x) }
   def denom(x) { return cdr(x) }

   def add_rat(x, y) {
      return make_rat((numer(x) * denom(y)) + (numer(y) * denom(x)), denom(x) * denom(y))
   }

   def sub_rat(x, y) {
      return make_rat((numer(x) * denom(y)) - (numer(y) * denom(x)), denom(x) * denom(y))
   }

   def mul_rat(x, y) {
      return make_rat(numer(x) * numer(y), denom(x) * denom(y))
   }

   def div_rat(x, y) {
      return make_rat(numer(x) * denom(y), denom(x) * numer(y))
   }

   def equal_rat(x, y) {
      return ((numer(x) * denom(y)) == (numer(y) * denom(x)))
   }

   var x := cons(1, 2)
   var y := cons(3, 4)
   var z := cons(x, y)
   println (car(car(z)))
   println (car(cdr(z)))

   # footnote -- alternative definitions
   var make_rat := cons
   var numer := car
   def compose(f, g) { return def _(x) { return f(g(x)) } }
   var denom := compose(car, cdr)

   def print_rat(x) {
      println (numer(x), "/", denom(x))
   }

   var one_half := make_rat(1, 2)
   print_rat(one_half)

   var one_third := make_rat(1, 3)
   print_rat(add_rat(one_half, one_third))
   print_rat(mul_rat(one_half, one_third))
   print_rat(add_rat(one_third, one_third))

   # reducing to lowest terms in constructor
   def make_rat(n, d) {
      var g := gcd(n, d)
      return cons(n // g, d // g)
   }

   def add_rat(x, y) {
      return make_rat((numer(x) * denom(y)) + (numer(y) * denom(x)), denom(x) * denom(y))
   }

   print_rat(add_rat(one_third, one_third))
# end Literal Translation #

# Object Translation #
   def makeRational() {
      def rational {
         to make_rat(n, d) { return cons(n, d) }
         to numer(x) { return car(x) }
         to denom(x) { return cdr(x) }
         to add_rat(x, y) {
            return rational.make_rat(
               (rational.numer(x) * rational.denom(y)) + (rational.numer(y) * rational.denom(x)),
               rational.denom(x) * rational.denom(y))
         }
         to sub_rat(x, y) {
            return rational.make_rat(
               (rational.numer(x) * rational.denom(y)) - (rational.numer(y) * rational.denom(x)),
               rational.denom(x) * rational.denom(y))
         }
         to mul_rat(x, y) {
            return rational.make_rat(rational.numer(x) * rational.numer(y), rational.denom(x) * rational.denom(y))
         }
         to div_rat(x, y) {
            return rational.make_rat(rational.numer(x) * rational.denom(y), rational.denom(x) * rational.numer(y))
         }
         to equal_rat(x, y) {
            return ((rational.numer(x) * rational.denom(y)) == (rational.numer(y) * rational.denom(x)))
         }
         to print_rat(x) {
             println (rational.numer(x), "/", rational.denom(x))
         }
      }
      return rational
   }

   var rational := makeRational()

   var one_half := rational.make_rat(1, 2)
   rational.print_rat(one_half)

   var one_third := rational.make_rat(1, 3)
   rational.print_rat(rational.add_rat(one_half, one_third))
   rational.print_rat(rational.mul_rat(one_half, one_third))
   rational.print_rat(rational.add_rat(one_third, one_third))

   # reducing to lowest terms in constructor
   def makeRational() {
      def rational {
         to make_rat(n, d) {
            var g := gcd(n, d)
            return cons(n // g, d // g)
         }
         to numer(x) { return car(x) }
         to denom(x) { return cdr(x) }
         to add_rat(x, y) {
            return rational.make_rat(
               (rational.numer(x) * rational.denom(y)) + (rational.numer(y) * rational.denom(x)),
               rational.denom(x) * rational.denom(y))
         }
         to sub_rat(x, y) {
            return rational.make_rat(
               (rational.numer(x) * rational.denom(y)) - (rational.numer(y) * rational.denom(x)),
               rational.denom(x) * rational.denom(y))
         }
         to mul_rat(x, y) {
            return rational.make_rat(rational.numer(x) * rational.numer(y), rational.denom(x) * rational.denom(y))
         }
         to div_rat(x, y) {
            return rational.make_rat(rational.numer(x) * rational.denom(y), rational.denom(x) * rational.numer(y))
         }
         to equal_rat(x, y) {
            return ((rational.numer(x) * rational.denom(y)) == (rational.numer(y) * rational.denom(x)))
         }
         to print_rat(x) {
            println (rational.numer(x), "/", rational.denom(x))
         }
      }
      return rational
   }
   var rational := makeRational()

   var one_third := rational.make_rat(1, 3)
   rational.print_rat(rational.add_rat(one_third, one_third))
# end Object Translation #


# 2.1.2 Introduction to Data Abstraction - Abstraction barriers

# Literal Translation #
   # reducing to lowest terms in selectors
   def make_rat(n, d) { return cons(n, d) }

   def numer(x) {
      var g := gcd(car(x), cadr(x))
      return car(x) // g
   }

   def denom(x) {
      var g := gcd(car(x), cadr(x))
      return cadr(x) // g
   }
# end Literal Translation #

# Object Translation #
   # reducing to lowest terms in selectors
   def makeRational() {
      def rational {
         to make_rat(n, d) { return cons(n, d) }
         to numer(x) {
            var n := car(x)
            var d := cadr(x)
            return n // gcd(n, d)
         }
         to denom(x) {
            var n := car(x)
            var d := cadr(x)
            return d // gcd(n, d)
         }
         to add_rat(x, y) {
            return rational.make_rat(
               (rational.numer(x) * rational.denom(y)) + (rational.numer(y) * rational.denom(x)),
               rational.denom(x) * rational.denom(y))
         }
         to sub_rat(x, y) {
            return rational.make_rat(
               (rational.numer(x) * rational.denom(y)) - (rational.numer(y) * rational.denom(x)),
               rational.denom(x) * rational.denom(y))
         }
         to mul_rat(x, y) {
            return rational.make_rat(rational.numer(x) * rational.numer(y), rational.denom(x) * rational.denom(y))
         }
         to div_rat(x, y) {
            return rational.make_rat(rational.numer(x) * rational.denom(y), rational.denom(x) * rational.numer(y))
         }
         to equal_rat(x, y) {
            return ((rational.numer(x) * rational.denom(y)) == (rational.numer(y) * rational.denom(x)))
         }
         to print_rat(x) {
             println (rational.numer(x), "/", rational.denom(x))
         }
      }
      return rational
   }
   var rational := makeRational()
# end Object Translation #

# Exercise 2.2 #
# exercise left to reader to define appropriate functions
# def print_point(p) {
#    println ("(" + str(x_point(p)) + "," + str(y_point(p)) + ")")
# }


# 2.1.3 Introduction to Data Abstraction - What is meant by data?
def cons(x, y) {
   def dispatchx(m) {
      return switch(m) {
         match == 0 { x }
         match == 1 { y }
         match _    { throw(`Argument not 0 or 1 -- CONS $m`) }
      }
   }
   return dispatchx
}

def car(z) { return z(0) }
def cdr(z) { return z(1) }

# Exercise 2.4 #
def cons(x, y) {
   return (def _(m) { return m(x, y) })
}
def car(z) {
   return z(def _(p, q) { return p })
}

# Exercise 2.6 #
var zero := def _(f) { return def _(x) { return x } }
def add1(n) { return def _(f) { def _(x) { return f((n(f))(x)) } } }

# 2.1.4 Introduction to Data Abstraction - Extended Exercise: Interval Arithmetic

# Literal Translation #
   def make_interval(a, b) { return cons(a, b) }

   def lower_bound(xs) { return car(xs) }
   def upper_bound(xs) { return cdr(xs) }

   def add_interval(x, y) {
      return make_interval(lower_bound(x) + lower_bound(y), upper_bound(x) + upper_bound(y))
   }

   def mul_interval(x, y) {
      var p1 := lower_bound(x) * lower_bound(y)
      var p2 := lower_bound(x) * upper_bound(y)
      var p3 := upper_bound(x) * lower_bound(y)
      var p4 := upper_bound(x) * upper_bound(y)
      return make_interval(
         min(min(p1, p2), min(p3, p4)),
         max(max(p1, p2), max(p3, p4)))
   }

   def div_interval(x, y) {
      var z := make_interval(1.0 / upper_bound(y), 1.0 / lower_bound(y))
      return mul_interval(x, z)
   }

   def make_center_width(c, w) {
      return make_interval(c-w, c+w)
   }

   def center(i) {
      return (lower_bound(i) + upper_bound(i)) / 2.0
   }

   def width(i) {
      return (upper_bound(i) - lower_bound(i)) / 2.0
   }

   # parallel resistors
   def par1(r1, r2) {
      return div_interval(mul_interval(r1, r2), add_interval(r1, r2))
   }

   def par2(r1, r2) {
      var one := make_interval(1.0, 1.0)
      return div_interval(one,
               add_interval(div_interval(one, r1),
                            div_interval(one, r2)))
   }
# end Literal Translation #

# Object Translation #
   def makeInterval() {
      def interval {
         to make_interval(a, b) { return cons(a, b) }
         to lower_bound(xs) { return car(xs) }
         to upper_bound(xs) { return cdr(xs) }
         to add_interval(x, y) {
            return interval.make_interval(interval.lower_bound(x) + interval.lower_bound(y),
                                          interval.upper_bound(x) + interval.upper_bound(y))
         }
         to mul_interval(x, y) {
            var p1 := interval.lower_bound(x) * interval.lower_bound(y)
            var p2 := interval.lower_bound(x) * interval.upper_bound(y)
            var p3 := interval.upper_bound(x) * interval.lower_bound(y)
            var p4 := interval.upper_bound(x) * interval.upper_bound(y)
            return interval.make_interval(
               min(min(p1, p2), min(p3, p4)),
               max(max(p1, p2), max(p3, p4)))
         }
         to div_interval(x, y) {
            var z := interval.make_interval(1.0 / interval.upper_bound(y), 1.0 / interval.lower_bound(y))
            return interval.mul_interval(x, z)
         }
         to make_center_width(c, w) {
            return interval.make_interval(c-w, c+w)
         }
         to center(i) {
            return (interval.lower_bound(i) + interval.upper_bound(i)) / 2.0
         }
         to width(i) {
            return (interval.upper_bound(i) - interval.lower_bound(i)) / 2.0
         }
      }
      return interval
   }
   var interval := makeInterval()

   # parallel resistors
   def par1(r1, r2) {
      return interval.div_interval(interval.mul_interval(r1, r2), interval.add_interval(r1, r2))
   }

   def par2(r1, r2) {
      var one := interval.make_interval(1.0, 1.0)
      return interval.div_interval(one,
               interval.add_interval(interval.div_interval(one, r1),
                                     interval.div_interval(one, r2)))
   }
# end Object Translation #

# 2.2.1 Hierarchical Data and the Closure Property - Representing Sequences

# same as above
# def car(xs) { return xs[0] }
# def cdr(xs) { return xs[1] }

cons(1, cons(2, cons(3, cons(4, nil))))
var one_through_four := list([1, 2, 3, 4])

println (one_through_four)
println (car(one_through_four))
println (cdr(one_through_four))
println (car(cdr(one_through_four)))
println (cons(10, one_through_four))

def list_ref(items, n) {
   if (n == 0) {
      return car(items)
   } else {
      return list_ref(cdr(items), n-1)
   }
}

var squares := list([1, 4, 9, 16, 25])
println (list_ref(squares, 3))

def length(items) {
   if (items == nil) {
      return 0
   } else {
      return 1 + length(cdr(items))
   }
}

var odds := list([1, 3, 5, 7])
println (length(odds))

def length(items) {
   def length_iter(a, count) {
      if (a == nil) {
         return count
      } else {
         return length_iter(cdr(a), 1+count)
      }
   }
   return length_iter(items, 0)
}

# really need to figure out the cons operator ::
def append(list1, list2) {
   if (list1 == nil) {
      return list2
   } else {
      return cons(car(list1), append(cdr(list1), list2))
   }
}

println (append(squares, odds))
println (append(odds, squares))

# Mapping over lists
def scale_list(factor, items) {
   if (items == nil) {
      return nil
   } else {
      return cons(car(items) * factor, scale_list(factor, cdr(items)))
   }
}

println (scale_list(10, list([1, 2, 3, 4, 5])))

# uncurried version of map
def map(proc, items) {
   if (items == nil) {
      return nil
   } else {
      return cons(proc(car(items)), map(proc, cdr(items)))
   }
}
println (map(abs, list([-10, 2.5, -11.6, 17])))
println (map(def _(x) { return x * x }, list([1, 2, 3, 4])))
def scale_list(factor, items) {
   return map(def _(x) { return x * factor }, items)
}
println (scale_list(10, list([1, 2, 3, 4, 5])))

# curried version map
def map(proc) {
   def map_lambda(items) {
      if (items == nil) {
         return nil
      } else {
         return cons(proc(car(items)), map (proc) (cdr(items)))
      }
   }
   return map_lambda
}
println (map (abs) (list([-10, 2.5, -11.6, 17])))
println (map (def _(x) { return x * x }) (list([1, 2, 3, 4])))
def scale_list(factor, items) {
   return map (def _(x) { return x * factor }) (items)
}
println (scale_list(10, list([1, 2, 3, 4, 5])))

# Not sure how to translate these to E?
#    (map + (list 1 2 3) (list 40 50 60) (list 700 800 900))
#    (map (lambda (x y) (+ x ( * 2 y))) (list 1 2 3) (list 4 5 6))

# Exercise 2.17 #
# exercise left to reader to define appropriate functions
# println (last_pair(list([23, 72, 149, 34])))

# Exercise 2.18 #
# exercise left to reader to define appropriate functions
# println (reverse(list([1, 4, 9, 16, 25])))

# Exercise 2.19 #
# exercise left to reader to define appropriate functions
# var us_coins := list([50, 25, 10, 5, 1])
# var uk_coins := list([100, 50, 20, 10, 5, 2, 1, 0.5])
# def cc(amount, coin_values) {
#    if (amount == 0) {
#       return 1
#    } else if ((amount < 0) or (no_more(coin_values))) {
#       return 0
#    } else {
#       return (cc(amount, except_first_denomination(coin_values)) +
#               cc(amount - first_denomination(coin_values), coin_values))
#    }
# }
# println (cc(100, us_coins))

# Exercise 2.20 #
# exercise left to reader to define appropriate functions
# println (same_parity(list([1, 2, 3, 4, 5, 6, 7])))
# println (same_parity(list([2, 3, 4, 5, 6, 7])))

# Exercise 2.21 #
# exercise left to reader to define appropriate functions
# println (square_list(list([1, 2, 3, 4])))

# Exercise 2.22 #
def square(x) { return x * x }
def square_list(items) {
   def iter(things, answer) {
      if (things == nil) {
         return answer
      } else {
         return iter(cdr(things), [square(car(things))] + answer)
      }
   }
   return iter(items, nil)
}

def square_list(items) {
   def iter(things, answer) {
      if (things == nil) {
         return answer
      } else {
         return iter(cdr(things), answer + [square(car(things))])
      }
   }
   return iter(items, nil)
}

# Exercise 2.23 #
def for_each(f, xs) {
   if (xs == nil) {
      return nil
   } else {
      f(car(xs))
   }
   return for_each(f, cdr(xs))
}
def printexp(s) {  }
for_each(println, list([57, 321, 88]))


# 2.2.2 Hierarchical Data and the Closure Property - Hierarchical Structures
def count_leaves(tree) {
   if (tree == nil) {
      return 0
   } else if (!islist(tree)) {
      return 1
   } else {
      return count_leaves(car(tree)) + count_leaves(cdr(tree))
   }
}

var x := list([list([1, 2]), list([3, 4])])
println (length(x))
println (count_leaves(x))

println (list([x, x]))
println (length(list([x, x])))
println (count_leaves(list([x, x])))

# Mapping over trees
def scale_tree(factor, tree) {
   if (tree == nil) {
      return nil
   } else if (!islist(tree)) {
      return tree * factor
   } else {
      return cons(scale_tree(factor, car(tree)), scale_tree(factor, cdr(tree)))
   }
}
println (scale_tree(10, list([1, list([2, list([3, 4]), 5]), list([6, 7])])))

def scale_tree(factor, tree) {
   return map(
      def _(sub_tree) {
         if (islist(sub_tree)) {
            return scale_tree(factor, sub_tree)
         } else {
            return sub_tree * factor
         }
      })(tree)
}
println (scale_tree(10, list([1, list([2, list([3, 4]), 5]), list([6, 7])])))

# Exercise 2.24 #
list([1, list([2, list([3, 4])])])

# Exercise 2.25 #
list([1, 3, list([5, 7]), 9])
list([list([7])])
list([1, list([2, list([3, list([4, list([5, list([6, 7])])])])])])

# Exercise 2.26 #
var x := list([1, 2, 3])
var y := list([4, 5, 6])
println (x + y)
println (list([x, y]))
println (list([x, y]))

# Exercise 2.27 #
var x := list([list([1, 2]), list([3, 4])])
# exercise left to reader to define appropriate functions
# reverse(x)
# deep_reverse(x)

# Exercise 2.28 #
var x := list([list([1, 2]), list([3, 4])])
# exercise left to reader to define appropriate functions
# fringe(x)
# fringe(list([x, x]))

# Exercise 2.29 #
def make_mobile(left, right) { return list([left, right]) }
def make_branch(length, struc) { return list([length, struc]) }

# Exercise 2.30 #
# exercise left to reader to define appropriate functions
# square_tree(list([1, list([2, list([3, 4]), 5]), list([6, 7])]))

# Exercise 2.31 #
# exercise left to reader to define appropriate functions
# def square_tree(tree) { return tree_map (square) (tree) }

# Exercise 2.32 #
# exercise left to reader to define appropriate functions
# def subsets(s) {
#    if (s == nil) {
#       return nil
#    } else {
#       var rest := subsets(cdr(s))
#       return append(rest, map (??FILL_THIS_IN??) (rest))
#    }
# }


# 2.2.3 Hierarchical Data and the Closure Property - Sequences as Conventional Interfaces
def isodd(n) { return ((n % 2) == 1) }
def iseven(n) { return ((n % 2) != 1) }

# same as above
#def square(x) { return x * x }

def sum_odd_squares(tree) {
   if (tree == nil) {
      return 0
   } else if (!islist(tree)) {
      if (isodd(tree)) {
         return square(tree)
      } else {
         return 0
      }
   } else {
      return (sum_odd_squares(car(tree)) +
              sum_odd_squares(cdr(tree)))
   }
}

def even_fibs(n) {
   def next(k) {
      if (k > n) {
         return nil
      } else {
         var f := fib(k)
         if (iseven(f)) {
            return cons(f, next(k+1))
         } else {
            return next(k+1)
         }
      }
   }
   return next(0)
}

println (sum_odd_squares(list([1,2,3,4,5])))
println (even_fibs(10))

# Sequence operations #
println (map (square) (list([1,2,3,4,5])))

# non-curried version of filter
def filter(predicate, sequence) {
   if (sequence == nil) {
      return nil
   } else {
      if (predicate(car(sequence))) {
         return cons(car(sequence), filter(predicate, cdr(sequence)))
      } else {
         return filter(predicate, cdr(sequence))
      }
   }
}
println (filter(isodd, list([1,2,3,4,5])))

# curried version of filter
def filter(predicate) {
   def filter_lambda(sequence) {
      if (sequence == nil) {
         return nil
      } else {
         if (predicate(car(sequence))) {
            return cons(car(sequence), filter (predicate) (cdr(sequence)))
         } else {
            return filter (predicate) (cdr(sequence))
         }
      }
   }
   return filter_lambda
}
println (filter (isodd) (list([1,2,3,4,5])))

# non-curried version of accumulate (aka foldl)
def accumulate(oper, initial, sequence) {
   if (sequence == nil) {
      return initial
   } else {
      return oper(car(sequence), accumulate(oper, initial, cdr(sequence)))
   }
}
println (accumulate(def _(x,y) { return x+y }, 0, list([1,2,3,4,5])))
println (accumulate(def _(x,y) { return x*y }, 1, list([1,2,3,4,5])))
println (accumulate(cons, nil, list([1,2,3,4,5])))

# curried version of accumulate (aka foldl)
def accumulate(oper) {
   def initial_lambda(initial) {
      def sequence_lambda(sequence) {
         if (sequence == nil) {
            return initial
         } else {
            return oper(car(sequence), accumulate (oper) (initial) (cdr(sequence)))
         }
      }
      return sequence_lambda
   }
   return initial_lambda
}
println (accumulate (def _(x,y) { return x+y }) (0) (list([1,2,3,4,5])))
println (accumulate (def _(x,y) { return x*y }) (1) (list([1,2,3,4,5])))
println (accumulate (cons) (nil) (list([1,2,3,4,5])))

def enumerate_interval(low, high) {
   if (low > high) {
      return nil
   } else {
      return cons(low, enumerate_interval(low+1, high))
   }
}

println (enumerate_interval(2,7))

def enumerate_tree(tree) {
   if (tree == nil) {
      return nil
   } else if (!islist(tree)) {
      return cons(tree, nil)
   } else {
      return (append(enumerate_tree(car(tree)),
                     enumerate_tree(cdr(tree))))
   }
}

println (enumerate_tree(list([1, list([2, list([3, 4]), 5])])))

def sum_odd_squares(tree) {
   return                               \
      (accumulate                       \
         (def _(x,y) { return x+y })    \
         (0)                            \
         (map                           \
            (square)                    \
            (filter                     \
               (isodd)                  \
               (enumerate_tree(tree)))))
}

def even_fibs(n) {
   return accumulate (cons) (nil) (
            filter (iseven) (map (fib) (enumerate_interval(0, n))))
}

def list_fib_squares(n) {
   return         \
      accumulate  \
         (cons)   \
         (nil)    \
         (map (square) (map (fib) (enumerate_interval(0, n))))
}

println (list_fib_squares(10))

def product_of_squares_of_odd_elements(sequence) {
   return                              \
      accumulate                       \
         (def _(x,y) { return x*y })   \
         (1)                           \
         (map (square) (filter (isodd) (sequence)))
}

println (product_of_squares_of_odd_elements(list([1,2,3,4,5])))

def makeEmployee(var empname, var jobtitle, var salary) {
   def employee {
      to isProgrammer() { return (jobtitle == "Programmer") }
      to getSalary() { return salary }
   }
   return employee
}

def salary_of_highest_paid_programmer(records) {
   return accumulate (max) (0) (
            map (def _(obj) { return obj.getSalary() }) (
               filter (def _(obj) { return obj.isProgrammer() }) (records)))
}

var recs := list([makeEmployee("Fred", "Programmer", 180),
                  makeEmployee("Hank", "Programmer", 150)])
println (salary_of_highest_paid_programmer(recs))

# Nested mappings
var n := 5                   # book doesn't define n
println (
   accumulate                                      \
      (append)                                     \
      (nil)                                        \
      (map                                         \
         (def _(i) {                               \
            return map                             \
               (def _(j) { return list([i, j]) })  \
               (enumerate_interval(1, i-1))        \
         })                                        \
         (enumerate_interval(1, n))))

def flatmap(proc) {
   def flatmap_lambda(seq) {
      return accumulate (append) (nil) (map (proc) (seq))
   }
   return flatmap_lambda
}

def has_no_divisors(n, c) {
   if (c == 1) {
      return true
   } else if ((n % c) == 0) {
      return false
   } else {
      return has_no_divisors(n, c-1)
   }
}

def isPrime(n) {
   return has_no_divisors(n, n-1)
}

def prime_sum(pair) {
   return isPrime(car(pair) + cadr(pair))
}

def make_pair_sum(pair) {
   return list([car(pair), cadr(pair), car(pair) + cadr(pair)])
}

def prime_sum_pairs(n) {
   return map                                                                                               \
            (make_pair_sum)                                                                                 \
            (filter                                                                                         \
               (prime_sum)                                                                                  \
               (flatmap                                                                                     \
                  (def _(i) { return map (def _(j) { return list([i,j]) }) (enumerate_interval(1, i-1)) })  \
                  (enumerate_interval(1, n))))
}

println (prime_sum_pairs(5))

def remove(item, sequence) {
   return filter (def _(x) { return x != item }) (sequence)
}

def permutations(s) {
   if (s == nil) {
      return list([nil])
   } else {
      return (                                        \
         flatmap                                      \
            (def _(x) {                               \
               return                                 \
                  map                                 \
                     (def _(p) { return cons(x, p) }) \
                     (permutations(remove(x, s)))     \
            })                                        \
            (s))
   }
}

println(permutations(list([1,2,3])))

# Exercise 2.34 #
# exercise left to reader to define appropriate functions
# def horner_eval(x, coefficient_sequence) {
#    return accumulate (def _(this_coeff, higher_terms) { return ??FILL_THIS_IN?? }) (0) (coefficient_sequence)
# }
# horner_eval(2, list([1,3,0,5,0,1]))

# Exercise 2.36 #
# exercise left to reader to define appropriate functions
# def accumulate_n(oper) {
#    def initial_lambda(initial) {
#       def sequence_lambda(sequence) {
#          if (sequence == nil) {
#             return initial
#          } else {
#             return cons(accumulate (oper) (init) (??FILL_THIS_IN??),
#                         accumulate_n (oper) (init) (??FILL_THIS_IN??))
#          }
#       }
#       return sequence_lambda
#    }
#    return initial_lambda
# }
# accumulate_n (def _(x,y) { return x + y }) (0) (s)

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
var fold_right := accumulate
def fold_left(oper) {
   def initial_lambda(initial) {
      def sequence_lambda(sequence) {
         def iter(result, xs) {
            if (xs == nil) {
               return result
            } else {
               return iter(oper(result, car(xs)), cdr(xs))
            }
         }
         return iter(initial, sequence)
      }
      return sequence_lambda
   }
   return initial_lambda
}
println (fold_right (def _(x,y) { return x/y }) (1) (list([1,2,3])))
println (fold_left (def _(x,y) { return x/y }) (1) (list([1,2,3])))
println (fold_right (cons) (nil) (list([1,2,3])))
println (fold_left (cons) (nil) (list([1,2,3])))

# Exercise 2.42 #
# exercise left to reader to define appropriate functions
# def queens(board_size) {
#    def queen_cols(k) {
#       if (k == 0) {
#          return list([empty_board])
#       } else {
#          return (                                                                                 /
#             filter                                                                                /
#                (def _(positions) { return isSafe(k, positions) })                                 /
#                (flatmap                                                                           /
#                   (def _(rest_of_queens) {                                                        /
#                      return                                                                       /
#                         map                                                                       /
#                           (fun _(new_row) { return adjoin_position(new_row, k, rest_of_queens) }) /
#                           (enumerate_interval(1, board_size))                                     /
#                   })                                                                              /
#                   (queen_cols(k-1))))
#       }
#    }
#    return queen_cols(board_size)
# }

# Exercise 2.43 #
# exercise left to reader to define appropriate functions
# def queens(board_size) {
#    def queen_cols(k) {
#       if (k == 0) {
#          return list([empty_board])
#       } else {
#          return (                                                                                         /
#             filter                                                                                        /
#                (def _(positions) { return isSafe(k, positions) })                                         /
#                (flatmap                                                                                   /
#                   (def _(new_row) {                                                                       /
#                      return                                                                               /
#                         map                                                                               /
#                            (def _(rest_of_queens) { return adjoin_position(new_row, k, rest_of_queens) }) /
#                            (queen_cols(k-1))                                                              /
#                   })                                                                                      /
#                   (enumerate_interval(1, board_size))))
#       }
#    }
#    return queen_cols(board_size)
# }


# 2.2.4 Hierarchical Data and the Closure Property - Example: a picture language

# these two routines are to be written #
def draw_line(x, y) { }
def wave(xframe) { return xframe }

def makeVect(var x, var y) {
   def vect {
      to getX() { return x }
      to getY() { return y }
   }
   return vect
}

def make_vect(x, y) { return makeVect(x, y) }
def xcor_vect(v) { return v.getX() }
def ycor_vect(v) { return v.getY() }
def add_vect(v1, v2) {
   return make_vect(xcor_vect(v1) + xcor_vect(v2), ycor_vect(v1) + ycor_vect(v2))
}
def sub_vect(v1, v2) {
   return make_vect(xcor_vect(v1) - xcor_vect(v2), ycor_vect(v1) - ycor_vect(v2))
}
def scale_vect(s, v) {
   return make_vect(s * xcor_vect(v), s * ycor_vect(v))
}

def makeFrame(var orig, var edge1, var edge2) {
   def frame {
      to getOrig() { return orig }
      to getEdge1() { return edge1 }
      to getEdge2() { return edge2 }
   }
   return frame
}

def make_frame(origin, edge1, edge2) {
   return makeFrame(origin, edge1, edge2)
}
def origin_frame(f) { return f.getOrig() }
def edge1_frame(f) { return f.getEdge1() }
def edge2_frame(f) { return f.getEdge2() }
var a_frame := make_frame(make_vect(0.0, 0.0), make_vect(1.0, 0.0), make_vect(0.0, 1.0))

def makeSegment(var x, var y) {
   def segment {
      to getX() { return x }
      to getY() { return y }
   }
   return segment
}

def start_segment(seg) { return seg.getX() }
def end_segment(seg) { return seg.getY() }

# Frames #
def frame_coord_map(xframe, v) {
   return add_vect(
      origin_frame(xframe),
      add_vect(scale_vect(xcor_vect(v), edge1_frame(xframe)),
               scale_vect(ycor_vect(v), edge2_frame(xframe))))
}

frame_coord_map(a_frame, make_vect(0.0, 0.0))
origin_frame(a_frame)

# Painters #
def foreach(f) {
   def foreach_lambda(xs) {
      if (x != nil) {
         f(car(xs))
         foreach (f) (cdr(xs))
      }
   }
   return foreach_lambda
}

def segments_painter(segment_list, xframe) {
   (foreach                                                      \
      (def _(segment) {                                          \
         draw_line(                                              \
            (frame_coord_map (xframe) (start_segment, segment)), \
            (frame_coord_map (xframe) (end_segment, segment)))   \
      })                                                         \
      (segment_list))
}

def transform_painter(painter, origin, corner1, corner2) {
   def transform_painter_lambda(xframe) {
      var m := frame_coord_map(xframe)
      var new_origin := m(origin)
      return painter(
         make_frame(
            new_origin,
            sub_vect(m(corner1), new_origin),
            sub_vect(m(corner2), new_origin)))
   }
   return transform_painter_lambda
}

def flip_vert(painter) {
   return transform_painter(
      painter,
      make_vect(0.0, 1.0),
      make_vect(1.0, 1.0),
      make_vect(0.0, 0.0))
}

def flip_horiz(painter) {
   return transform_painter(
      painter,
      make_vect(1.0, 0.0),
      make_vect(0.0, 0.0),
      make_vect(1.0, 1.0))
}

def shrink_to_upper_right(painter) {
   return transform_painter(
      painter,
      make_vect(0.5, 0.5),
      make_vect(1.0, 0.5),
      make_vect(0.5, 1.0))
}

def rotate90(painter) {
   return transform_painter(
      painter,
      make_vect(1.0, 0.0),
      make_vect(1.0, 1.0),
      make_vect(0.0, 0.0))
}

def rotate180(painter) {
   return transform_painter(
      painter,
      make_vect(1.0, 1.0),
      make_vect(0.0, 1.0),
      make_vect(1.0, 0.0))
}

def squash_inwards(painter) {
   return transform_painter(
      painter,
      make_vect(0.0, 0.0),
      make_vect(0.65, 0.35),
      make_vect(0.35, 0.65))
}

def beside(painter1, painter2) {
   def beside_lambda(xframe) {
      var split_point := make_vect(0.5, 0.0)
      var paint_left := (
         transform_painter(
            painter1,
            make_vect(0.0, 0.0),
            split_point,
            make_vect(0.0, 1.0)))
      var paint_right := (
         transform_painter(
            painter2,
            split_point,
            make_vect(1.0, 0.0),
            make_vect(0.5, 1.0)))
      paint_left(xframe)
      paint_right(xframe)
   }
   return beside_lambda
}

def below(painter1, painter2) {
   def below_lambda(xframe) {
      var split_point := make_vect(0.0, 0.5)
      var paint_below := (
         transform_painter(
            painter1,
            make_vect(0.0, 0.0),
            make_vect(1.0, 0.0),
            split_point))
      var paint_above := (
         transform_painter(
            painter2,
            split_point,
            make_vect(1.0, 0.5),
            make_vect(0.0, 1.0)))
      paint_below(xframe)
      paint_above(xframe)
   }
   return below_lambda
}

def up_split(painter, n) {
   if (n == 0) {
      return painter
   } else {
      var smaller := up_split(painter, n-1)
      return below(painter, beside(smaller, smaller))
   }
}

var wave2 := beside(wave, flip_vert(wave))

var wave4 := below(wave2, wave)

def flipped_pairs(painter) {
   var painter2 := beside(painter, flip_vert(painter))
   return below(painter2, painter2)
}

var wave4_ := flipped_pairs(wave)

def right_split(painter, n) {
   if (n == 0) {
      return painter
   } else {
      var smaller := right_split(painter, n-1)
      return beside(painter, below(smaller, smaller))
   }
}

def corner_split(painter, n) {
   if (n == 0) {
      return painter
   } else {
      var up := up_split(painter, n-1)
      var right := right_split(painter, n-1)
      var top_left := beside(up, up)
      var bottom_right := below(right, right)
      var corner := corner_split(painter, n-1)
      return beside(below(painter, top_left),  below(bottom_right, corner))
   }
}

def square_limit(painter, n) {
   var quarter := corner_split(painter, n)
   var half := beside(flip_horiz(quarter), quarter)
   return below(flip_vert(half), half)
}

# Higher_order operations #
def square_of_four(tleft, tright, bleft, bright) {
   def square_of_four_lambda(painter) {
      var top := beside(tleft(painter), tright(painter))
      var bottom := beside(bright(painter), bright(painter))
      return below(bottom, top)
   }
   return square_of_four_lambda
}

def flipped_pairs(painter) {
   var combine4 := square_of_four(identity, flip_vert, identity, flip_vert)
   return combine4(painter)
}

# footnote #
var flipped_pairs := square_of_four(identity, flip_vert, identity, flip_vert)

def square_limit(painter, n) {
   var combine4 := square_of_four(flip_horiz, identity, rotate180, flip_vert)
   return combine4(corner_split(painter, n))
}

# Exercise 2.45 #
# exercise left to reader to define appropriate functions
# var right_split := split(beside, below)
# var up_split := split(below, beside)

# Exercise 2.47 #
def make_frame(origin, edge1, edge2) {
   return [origin, edge1, edge2]
}
def make_frame(origin, edge1, edge2) {
   return [origin, [edge1, edge2]]
}

# 2.3.1 Symbolic Data - Quotation
# To Be Done
