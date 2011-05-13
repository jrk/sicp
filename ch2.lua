-- Note: The list primitives are a work in progress.
--       Although they come close to capturing the
--       Scheme's list type, there are still a couple
--       of holes that need to be plugged.  In addition
--       I'd like to make the code a bit closer to
--       Lua idioms.

-- a set of list primitives (Courtesy Rici Lake)
function cons(h, t)
  return function() return h, t end
end

function flip(x, y) return y, x end

function car(xs)
   -- The redundant () forces the return to be one value
   return (xs())
end

function cdr(xs)
   return (flip(xs()))
end

function cadr(xs)
   return car(cdr(xs))
end

function list(dict)
   local c = nil
   if (table.getn(dict) == 0) then
      c = cons(nil, c)
   end
   for i = (table.getn(dict)), 1, -1 do
      c = cons(dict[i], c)
   end
   return c
end

-- I need a better way to tag the structure as a list - this will work for now
function islist(xs)
   return type(xs) == "function"
end

function printlist(xs)
   local function printval(xs)
      local function iter(xs)
         if (xs == nil) then
            return ""
         else
            if (cdr(xs) == nil) then
               return printval(car(xs))
            else
               return printval(car(xs)) .. "," .. iter(cdr(xs))
            end
         end
      end
      if ((xs == nil) or (islist(xs))) then
         return "[" .. iter(xs) .. "]"
      else
         return xs
      end
   end
   print (printval(xs))
end


-- Functions defined in previous chapters --
function gcd(a, b)
   if (b == 0) then
      return a
   else
      return gcd(b, math.mod(a, b))
   end
end

function fib(n)
   if (n == 0) then
      return 0
   elseif (n == 1) then
      return 1
   else
      return fib(n - 1) + fib(n - 2)
   end
end

function identity(x) return x end


-- 2 Building Abstractions with Data
function linear_combination(a, b, x, y)
   return (a * x) + (b * y)
end

function mul(a, b) return a * b end
function linear_combination(a, b, x, y)
   return mul(a, x) + mul(b, y)
end

-- 2.1.1 Introduction to Data Abstraction - Example: Arithmetic Operations for Rational Numbers

-- Literal Translation --
   function make_rat(n, d) return cons(n, d) end
   function numer(x) return car(x) end
   function denom(x) return cdr(x) end

   function add_rat(x, y)
      return make_rat((numer(x) * denom(y)) + (numer(y) * denom(x)), denom(x) * denom(y))
   end

   function sub_rat(x, y)
      return make_rat((numer(x) * denom(y)) - (numer(y) * denom(x)), denom(x) * denom(y))
   end

   function mul_rat(x, y)
      return make_rat(numer(x) * numer(y), denom(x) * denom(y))
   end

   function div_rat(x, y)
      return make_rat(numer(x) * denom(y), denom(x) * numer(y))
   end

   function equal_rat(x, y)
      return ((numer(x) * denom(y)) == (numer(y) * denom(x)))
   end

   x = cons(1, 2)
   y = cons(3, 4)
   z = cons(x, y)
   print (car(car(z)))
   print (car(cdr(z)))

   --footnote -- alternative definitions
   make_rat = cons
   numer = car
   denom = cdr

   x = cons(1, 2)
   y = cons(3, 4)
   print (numer(x))
   print (denom(x))

   function print_rat(x)
      print (string.format("%d/%d", numer(x), denom(x)))
   end

   one_half = make_rat(1,2)
   print_rat(one_half)

   one_third = make_rat(1, 3)
   print_rat(add_rat(one_half, one_third))
   print_rat(mul_rat(one_half, one_third))
   print_rat(add_rat(one_third, one_third))

   -- reducing to lowest terms in constructor
   function make_rat(n, d)
      local g = gcd(n, d)
      return cons(n / g, d / g)
   end

   function add_rat(x, y)
      return make_rat((numer(x) * denom(y)) + (numer(y) * denom(x)), denom(x) * denom(y))
   end

   print_rat(add_rat(one_third, one_third))
-- end Literal Translation --

-- Object Translation --
   function Rational()
      return {
         make_rat = function(self, n, d) return cons(n, d) end,
         numer = function(self, x) return car(x) end,
         denom = function(self, x) return cdr(x) end,
         add_rat = function(self, x, y)
            return self:make_rat(
               (self:numer(x) * self:denom(y)) + (self:numer(y) * self:denom(x)),
                self:denom(x) * self:denom(y))
         end,
         sub_rat = function(self, x, y)
            return self:make_rat(
               (self:numer(x) * self:denom(y)) - (self:numer(y) * self:denom(x)),
                self:denom(x) * self:denom(y))
         end,
         mul_rat = function(self, x, y)
            return self:make_rat(self:numer(x) * self:numer(y), self:denom(x) * self:denom(y))
         end,
         div_rat = function(self, x, y)
            return self:make_rat(self:numer(x) * self:denom(y), self:denom(x) * self:numer(y))
         end,
         equal_rat = function(self, x, y)
            return ((self:numer(x) * self:denom(y)) == (self:numer(y) * self:denom(x)))
         end,
         print_rat = function(self, x)
            print (string.format("%d/%d", self:numer(x), self:denom(x)))
         end
      }
   end
   rational = Rational()

   one_half = rational:make_rat(1, 2)
   rational:print_rat(one_half)

   one_third = rational:make_rat(1, 3)
   rational:print_rat(rational:add_rat(one_half, one_third))
   rational:print_rat(rational:mul_rat(one_half, one_third))
   rational:print_rat(rational:add_rat(one_third, one_third))

   -- reducing to lowest terms in constructor
   function Rational()
      return {
         make_rat = function(self, n, d)
            local g = gcd(n, d)
            return cons(n / g, d / g)
         end,
         numer = function(self, x) return car(x) end,
         denom = function(self, x) return cdr(x) end,
         add_rat = function(self, x, y)
            return self:make_rat(
               (self:numer(x) * self:denom(y)) + (self:numer(y) * self:denom(x)),
                self:denom(x) * self:denom(y))
         end,
         sub_rat = function(self, x, y)
            return self:make_rat(
               (self:numer(x) * self:denom(y)) - (self:numer(y) * self:denom(x)),
                self:denom(x) * self:denom(y))
         end,
         mul_rat = function(self, x, y)
            return self:make_rat(self:numer(x) * self:numer(y), self:denom(x) * self:denom(y))
         end,
         div_rat = function(self, x, y)
            return self:make_rat(self:numer(x) * self:denom(y), self:denom(x) * self:numer(y))
         end,
         equal_rat = function(self, x, y)
            return ((self:numer(x) * self:denom(y)) == (self:numer(y) * self:denom(x)))
         end,
         print_rat = function(self, x)
            print (string.format("%d/%d", self:numer(x), self:denom(x)))
         end
      }
   end
   rational = Rational()

   one_third = rational:make_rat(1, 3)
   rational:print_rat(rational:add_rat(one_third, one_third))
-- end Object Translation --


-- 2.1.2 Introduction to Data Abstraction - Abstraction barriers

-- Literal Translation --
   -- reducing to lowest terms in selectors
   function make_rat(n, d) return cons(n, d) end

   function numer(x)
      local g = gcd(car(x), cdr(x))
      return car(x) / g
   end

   function denom(x)
      local g = gcd(car(x), cdr(x))
      return cdr(x) / g
   end
-- end Literal Translation --

-- Object Translation --
   -- reducing to lowest terms in selectors
   function Rational()
      return {
         make_rat = function(self, n, d) return cons(n, d) end,
         numer = function(self, x)
            local g = gcd(car(x), cdr(x))
            return car(x) / g
         end,
         denom = function(self, x)
            local g = gcd(car(x), cdr(x))
            return cdr(x) / g
         end,
         add_rat = function(self, x, y)
            return self:make_rat(
               (self:numer(x) * self:denom(y)) + (self:numer(y) * self:denom(x)),
                self:denom(x) * self:denom(y))
         end,
         sub_rat = function(self, x, y)
            return self:make_rat(
               (self:numer(x) * self:denom(y)) - (self:numer(y) * self:denom(x)),
                self:denom(x) * self:denom(y))
         end,
         mul_rat = function(self, x, y)
            return self:make_rat(self:numer(x) * self:numer(y), self:denom(x) * self:denom(y))
         end,
         div_rat = function(self, x, y)
            return self:make_rat(self:numer(x) * self:denom(y), self:denom(x) * self:numer(y))
         end,
         equal_rat = function(self, x, y)
            return ((self:numer(x) * self:denom(y)) == (self:numer(y) * self:denom(x)))
         end,
         print_rat = function(self, x)
            print (string.format("%d/%d", self:numer(x), self:denom(x)))
         end
      }
   end
   rational = Rational()
-- end Object Translation --

-- Exercise 2.2 --
-- exercise left to reader to define appropriate functions
-- function print_point(p)
--    print (string.format("(%d,%d)", x_point(p), y_point(p)))
-- end

-- 2.1.3 Introduction to Data Abstraction - What is meant by data?

function cons_2(x, y)
   local function dispatch(m)
      if (m == 0) then
         return x
      elseif (m == 1) then
         return y
      else
         error("Argument not 0 or 1 -- CONS " .. m)
      end
   end
   return dispatch
end

function car_1(z) return z(0) end
function cdr_1(z) return z(1) end

-- Exercise 2.4 --
function cons_3(x, y)
   return (function(m) return m(x, y) end)
end
function car_2(z)
   return z(function(p, q) return p end)
end

-- Exercise 2.6 --
zero = function(f) return function(x) return x end end
function add1(n)
  return function(f)
    return function(x)
      return f(n(f)(x))
    end
  end
end


-- 2.1.4 Introduction to Data Abstraction - Extended Exercise: Interval Arithmetic

-- Literal Translation --
   function make_interval(a, b) return cons(a, b) end

   function lower_bound(interval) return car(interval) end

   function upper_bound(interval) return cdr(interval) end

   function add_interval(x, y)
      return make_interval(lower_bound(x) + lower_bound(y), upper_bound(x) + upper_bound(y))
   end

   function mul_interval(x, y)
      local p1 = lower_bound(x) * lower_bound(y)
      local p2 = lower_bound(x) * upper_bound(y)
      local p3 = upper_bound(x) * lower_bound(y)
      local p4 = upper_bound(x) * upper_bound(y)
      return make_interval(
         min(min(p1, p2), min(p3, p4)),
         max(max(p1, p2), max(p3, p4)))
   end

   function div_interval(x, y)
      local z = make_interval(1.0 / upper_bound(y), 1.0 / lower_bound(y))
      return mul_interval(x, z)
   end

   function make_center_width(c, w)
      return make_interval(c-w, c+w)
   end

   function center(interval)
      return (lower_bound(interval) + upper_bound(interval)) / 2.0
   end

   function width(interval)
      return (upper_bound(interval) - lower_bound(interval)) / 2.0
   end

   -- parallel resistors
   function par1(r1, r2)
      return div_interval(mul_interval(r1, r2), add_interval(r1, r2))
   end

   function par2(r1, r2)
      local one = make_interval(1.0, 1.0)
      return div_interval(one,
               add_interval(div_interval(one, r1),
                            div_interval(one, r2)))
   end
-- end Literal Translation --

-- Object Translation --
   function Interval()
      return {
         make_interval = function(self, a, b) return cons(a, b) end,
         lower_bound = function(self, interval) return car(interval) end,
         upper_bound = function(self, interval) return cdr(interval) end,
         add_interval = function(self, x, y)
            return self:make_interval(self:lower_bound(x) + self:lower_bound(y), self:upper_bound(x) + self:upper_bound(y))
         end,
         mul_interval = function(self, x, y)
            local p1 = self:lower_bound(x) * self:lower_bound(y)
            local p2 = self:lower_bound(x) * self:upper_bound(y)
            local p3 = self:upper_bound(x) * self:lower_bound(y)
            local p4 = self:upper_bound(x) * self:upper_bound(y)
            return self:make_interval(
               min(min(p1, p2), min(p3, p4)),
               max(max(p1, p2), max(p3, p4)))
         end,
         div_interval = function(self, x, y)
            local z = self:make_interval(1.0 / self:upper_bound(y), 1.0 / self:lower_bound(y))
            return self:mul_interval(x, z)
         end,
         make_center_width = function(self, c, w)
            return self:make_interval(c-w, c+w)
         end,
         center = function(self, interval)
            return (self:lower_bound(interval) + self:upper_bound(interval)) / 2.0
         end,
         width = function(self, interval)
            return (self:upper_bound(interval) - self:lower_bound(interval)) / 2.0
         end
      }
   end
   interval = Interval()

   -- parallel resistors
   function par1(r1, r2)
      return interval:div_interval(interval:mul_interval(r1, r2), interval:add_interval(r1, r2))
   end

   function par2(r1, r2)
      local one = interval:make_interval(1.0, 1.0)
      return interval:div_interval(one,
               interval:add_interval(interval:div_interval(one, r1),
                                     interval:div_interval(one, r2)))
   end
-- end Object Translation --

-- 2.2.1 Hierarchical Data and the Closure Property - Representing Sequences

cons(1, cons(2, cons(3, cons(4, nil))))
one_through_four = list({1, 2, 3, 4})

printlist (one_through_four)
print (car(one_through_four))
printlist (cdr(one_through_four))
print (car(cdr(one_through_four)))
printlist (cons(10, one_through_four))

function list_ref(items, n)
   if (n == 0) then
      return car(items)
   else
      return list_ref(cdr(items), n-1)
   end
end

squares = list({1, 4, 9, 16, 25})
print (list_ref(squares, 3))

function length(items)
   if (items == nil) then
      return 0
   else
      return 1 + length(cdr(items))
   end
end

odds = list({1, 3, 5, 7})
print (length(odds))

function length(items)
   local function length_iter(a, count)
      if (a == nil) then
         return count
      else
         return length_iter(cdr(a), 1+count)
      end
   end
   return length_iter(items, 0)
end

-- really need to figure out the cons operator ::
function append(list1, list2)
   if (list1 == nil) then
      return list2
   else
      return cons(car(list1), append(cdr(list1), list2))
   end
end

printlist (append(list({1, 3, 5, 7}), list({8,9,10})))

printlist (append(squares, odds))
printlist (append(odds, squares))

-- Mapping over lists
function scale_list(factor, items)
   if (items == nil) then
      return nil
   else
      return cons((car(items) * factor), scale_list(factor, cdr(items)))
   end
end

printlist (scale_list(10, list({1, 2, 3, 4, 5})))

-- uncurried version of map
function map(proc, items)
   if (items == nil) then
      return nil
   else
      return cons(proc(car(items)), map(proc, cdr(items)))
   end
end
printlist (map(math.abs, list({-10, 2.5, -11.6, 17})))
printlist (map(function(x) return x * x end, list({1, 2, 3, 4})))

function scale_list(factor, items)
   return map(function(x) return x * factor end, items)
end

-- curried version map
function map(proc)
   local function map_lambda(items)
      if (items == nil) then
         return nil
      else
         return cons(proc(car(items)), map (proc) (cdr(items)))
      end
   end
   return map_lambda
end
printlist (map (math.abs) (list({-10, 2.5, -11.6, 17})))
printlist (map (function(x) return x * x end) (list({1, 2, 3, 4})))

function scale_list(factor, items)
   return map (function(x) return x * factor end) (items)
end

-- Not sure how to translate these to Lua?
--    (map + (list 1 2 3) (list 40 50 60) (list 700 800 900))
--    (map (lambda (x y) (+ x ( * 2 y))) (list 1 2 3) (list 4 5 6))

-- Exercise 2.17 --
-- exercise left to reader to define appropriate functions
-- printlist (last_pair(list({23, 72, 149, 34})))

-- Exercise 2.18 --
-- exercise left to reader to define appropriate functions
-- printlist (reverse(list({1, 4, 9, 16, 25})))

-- Exercise 2.19 --
-- exercise left to reader to define appropriate functions
-- us_coins = list({50, 25, 10, 5, 1})
-- uk_coins = list({100, 50, 20, 10, 5, 2, 1, 0.5})
-- function cc(amount, coin_values)
--    if (amount == 0) then
--       return 1
--    elseif ((amount < 0) or (no_more(coin_values))) then
--       return 0
--    else
--       return (cc(amount, except_first_denomination(coin_values)) +
--               cc(amount - first_denomination(coin_values), coin_values))
--    end
-- end
-- print (cc(100, us_coins))

-- Exercise 2.20 --
-- exercise left to reader to define appropriate functions
-- print (same_parity(list({1, 2, 3, 4, 5, 6, 7})))
-- print (same_parity(list({2, 3, 4, 5, 6, 7})))

-- -- Exercise 2.21 --
-- -- exercise left to reader to define appropriate functions
-- -- printlist (square_list(list({1, 2, 3, 4})))

-- Exercise 2.22 --
function square(x) return x * x end
function square_list(items)
   local function iter(things, answer)
      if (things == nil) then
         return answer
      else
         return iter(cdr(things), cons(square(car(things)), answer))
      end
   end
   return iter(items, nil)
end
function square_list(items)
   local function iter(things, answer)
      if (things == nil) then
         return answer
      else
         return iter(cdr(things), append(answer, list({square(car(things))})))
      end
   end
   return iter(items, nil)
end

-- Exercise 2.23 --
function for_each(f, xs)
   if (xs == nil) then
      return nil
   else
      f(car(xs))
      return for_each(f, cdr(xs))
   end
end
for_each(print, list({57, 321, 88}))

-- 2.2.2 Hierarchical Data and the Closure Property - Hierarchical Structures

function count_leaves(tree)
   if (tree == nil) then
      return 0
   elseif not(islist(tree)) then
      return 1
   else
      return count_leaves(car(tree)) + count_leaves(cdr(tree))
   end
end

x = list({list({1, 2}), list({3, 4})})
print (length(x))
print (count_leaves(x))

printlist (list({x, x}))
print (length(list({x, x})))
print (count_leaves(list({x, x})))

-- Mapping over trees
function scale_tree(factor, tree)
   if (tree == nil) then
      return nil
   elseif not(islist(tree)) then
      return tree * factor
   else
      return cons(scale_tree(factor, car(tree)), scale_tree(factor, cdr(tree)))
   end
end

printlist (scale_tree(10, list({1, list({2, list({3, 4}), 5}), list({6, 7})})))

function scale_tree(factor, tree)
   return map(
         function(sub_tree)
            if (islist(sub_tree)) then
               return scale_tree(factor, sub_tree)
            else
               return sub_tree * factor
            end
         end
      ) (tree)
end

printlist (10, scale_tree(list({1, list({2, list({3, 4}), 5}), list({6, 7})})))

-- Exercise 2.24 --
list({1, list({2, list({3, 4})})})

-- Exercise 2.25 --
list({1, 3, list({5, 7}), 9})
list({list({7})})
list({1, list({2, list({3, list({4, list({5, list({6, 7})})})})})})

-- Exercise 2.26 --
x = list({1, 2, 3})
y = list({4, 5, 6})
printlist (cons(x, y))
printlist (list({x, y}))
printlist (list({x, y}))

-- Exercise 2.27 --
x = list({list({1, 2}), list({3, 4})})
-- exercise left to reader to define appropriate functions
-- reverse(x)
-- deep_reverse(x)

-- Exercise 2.28 --
x = list({list({1, 2}), list({3, 4})})
-- exercise left to reader to define appropriate functions
-- fringe(x)
-- fringe(list({x, x}))

-- Exercise 2.29 --
function make_mobile(left, right) return list({left, right}) end
function make_branch(length, struc) return list({length, struc}) end

-- Exercise 2.30 --
-- exercise left to reader to define appropriate functions
-- square_tree(list({1, list({2, list({3, 4}), 5}), list({6, 7})}))

-- Exercise 2.31 --
-- exercise left to reader to define appropriate functions
-- function square_tree(tree) return tree_map (square) (tree) end

-- Exercise 2.32 --
-- exercise left to reader to define appropriate functions
-- function subsets(s)
--    if (s == nil) then
--       return nil
--    else
--       local rest = subsets(cdr(s))
--       return append(rest, map (??FILL_THIS_IN??) (rest))
--    end
-- end

-- 2.2.3 Hierarchical Data and the Closure Property - Sequences as Conventional Interfaces
function isodd(n) return (math.mod(n, 2) == 1) end
function iseven(n) return (math.mod(n, 2) ~= 1) end
function square(x) return x * x end

function sum_odd_squares(tree)
   if (tree == nil) then
      return 0
   elseif not(islist(tree)) then
      if (isodd(tree)) then
         return square(tree)
      else
         return 0
      end
   else
      return (sum_odd_squares(car(tree)) +
              sum_odd_squares(cdr(tree)))
   end
end

function even_fibs(n)
   local function next(k)
      if (k > n) then
         return nil
      else
         local f = fib(k)
         if (iseven(f)) then
            return cons(f, next(k+1))
         else
            return next(k+1)
         end
      end
   end
   return next(0)
end

-- Sequence operations --
printlist (map (square) (list({1,2,3,4,5})))

-- non-curried version of filter
function filter(predicate, sequence)
   if (sequence == nil) then
      return nil
   else
      if (predicate(car(sequence))) then
         return cons(car(sequence), filter(predicate, cdr(sequence)))
      else
         return filter(predicate, cdr(sequence))
      end
   end
end

printlist (filter(isodd, list({1,2,3,4,5})))

-- curried version of filter
function filter(predicate)
   local function filter_lambda(sequence)
      if (sequence == nil) then
         return nil
      else
         if (predicate(car(sequence))) then
            return cons(car(sequence), filter (predicate) (cdr(sequence)))
         else
            return filter (predicate) (cdr(sequence))
         end
      end
   end
   return filter_lambda
end

printlist (filter (isodd) (list({1,2,3,4,5})))

-- non-curried version of accumulate (aka foldl)
function accumulate(oper, initial, sequence)
   if (sequence == nil) then
      return initial
   else
      return oper(car(sequence), accumulate(oper, initial, cdr(sequence)))
   end
end

print (accumulate(function(x,y) return x+y end, 0, list({1,2,3,4,5})))
print (accumulate(function(x,y) return x*y end, 1, list({1,2,3,4,5})))
printlist (accumulate(cons, nil, list({1,2,3,4,5})))

-- curried version of accumulate (aka foldl)
function accumulate(oper)
   local function initial_lambda(initial)
      local function sequence_lambda(sequence)
         if (sequence == nil) then
            return initial
         else
            return oper(car(sequence), accumulate (oper) (initial) (cdr(sequence)))
         end
      end
      return sequence_lambda
   end
   return initial_lambda
end

print (accumulate (function(x,y) return x+y end) (0) (list({1,2,3,4,5})))
print (accumulate (function(x,y) return x*y end) (0) (list({1,2,3,4,5})))
printlist (accumulate (cons) (nil) (list({1,2,3,4,5})))

function enumerate_interval(low, high)
   if (low > high) then
      return nil
   else
      return cons(low, enumerate_interval(low+1, high))
   end
end

printlist (enumerate_interval(2,7))

function enumerate_tree(tree)
   if (tree == nil) then
      return nil
   elseif not(islist(tree)) then
      return list({tree})
   else
      return (append(enumerate_tree(car(tree)),
                     enumerate_tree(cdr(tree))))
   end
end

printlist (enumerate_tree(list({1, list({2, list({3, 4}), 5})})))

function sum_odd_squares(tree)
   return
      accumulate (
         function(x,y) return x+y end) (
         0) (
         map (square) (filter (isodd) (enumerate_tree(tree))))
end

function even_fibs(n)
   return accumulate (cons) (nil) (filter (iseven) (map (fib) (enumerate_interval(0, n))))
end

function list_fib_squares(n)
   return accumulate (cons) (nil) (map (square) (map (fib) (enumerate_interval(0, n))))
end

printlist (list_fib_squares(10))

function product_of_squares_of_odd_elements(sequence)
   return accumulate (function(x,y) return x*y end) (1) (map (square) (filter (isodd) (sequence)))
end

print (product_of_squares_of_odd_elements(list({1,2,3,4,5})))

function Employee(init_empname, init_jobtitle, init_salary)
   local emp = {}
   emp.empname = init_empname
   emp.jobtitle = init_jobtitle
   emp.salary = init_salary
   return emp
end

function isProgrammer(emp)
   return (emp.jobtitle == "Programmer")
end

function getSalary(emp)
   return emp.salary
end

function salary_of_highest_paid_programmer(records)
   return accumulate (math.max) (0) (map (getSalary) (filter (isProgrammer) (records)))
end

recs = list({Employee("Fred", "Programmer", 180),
             Employee("Hank", "Programmer", 150)})

print (salary_of_highest_paid_programmer(recs))

-- Nested mappings
n = 5                   -- book doesn't define n
printlist (accumulate (append) (nil) (
            map (
               function(i)
                  return map (function(j) return list({i,j}) end) (enumerate_interval(1, i-1))
                end) (
            enumerate_interval(1, n))))

function flatmap(proc)
   return
      function(seq)
         return accumulate (append) (nil) (map (proc) (seq))
      end
end

function has_no_divisors(n, c)
    if (c == 1) then
       return true
    elseif (math.mod(n, c) == 0) then
       return false
    else
       return has_no_divisors(n, c-1)
    end
end

function isPrime(n)
   return has_no_divisors(n, n-1)
end

function prime_sum(pair)
   return isPrime(car(pair) + cadr(pair))
end

function make_pair_sum(pair)
   return list({car(pair), cadr(pair), car(pair) + cadr(pair)})
end

function prime_sum_pairs(n)
   return
      map (make_pair_sum)(
           filter (
               prime_sum) (
               flatmap (
                  function(i) return map (function(j) return list({i,j}) end) (enumerate_interval(1, i-1)) end) (
                  enumerate_interval(1, n))))
end

printlist (prime_sum_pairs(15))

function remove(item, sequence)
   return filter (function(x) return x ~= item end) (sequence)
end

function permutations(s)
   if (s == nil) then
      return list({nil})
   else
      printlist(s)
      return
         flatmap (
            function(x)
               return map (
                  function(p) return cons(x, p) end) (
                  permutations(remove(x, s)))
            end) (s)
   end
end

printlist (permutations(list({1,2,3})))

-- Exercise 2.34 --
-- exercise left to reader to define appropriate functions
-- function horner_eval(x, coefficient_sequence)
--    return accumulate (function(this_coeff, higher_terms) return ??FILL_THIS_IN?? end) (0) (coefficient_sequence)
-- end
-- horner_eval(2, list({1,3,0,5,0,1}))

-- Exercise 2.36 --
-- exercise left to reader to define appropriate functions
-- function accumulate_n(oper)
--    local function initial_lambda(initial)
--       local function sequence_lambda(sequence)
--          if (sequence == nil) then
--             return initial
--          else
--             return (cons(accumulate (oper) (init) (??FILL_THIS_IN??),
--                          accumulate_n (oper) (init) (??FILL_THIS_IN??)))
--          end
--       end
--       return sequence_lambda
--    end
--    return initial_lambda
-- end
--accumulate_n (lambda x,y: x + y) (0) (s)

-- CMR Error - need to finish this one --
--  -- Exercise 2.37 *
--  function dot_product(v, w) =
--     accumulate
--        op+
--        0
--        (map
--              (fn i =>
--                 map
--                    (fn j => i * j)
--                    w)
--              v)

-- Exercise 2.38 --
fold_right = accumulate
function fold_left(oper)
   local function initial_lambda(initial)
      local function sequence_lambda(sequence)
         local function iter(result, xs)
            if (xs == nil) then
               return result
            else
               return iter(oper(result, car(xs)), cdr(xs))
            end
         end
         return iter(initial, sequence)
      end
      return sequence_lambda
   end
   return initial_lambda
end
print (fold_right (function(x,y) return x/y end) (1.0) (list({1.0,2.0,3.0})))
print (fold_left (function(x,y) return x/y end) (1.0) (list({1.0,2.0,3.0})))
printlist (fold_right (cons) (nil) (list({1,2,3})))
-- CMR Error - runtime error - Scheme result = (((() 1) 2) 3) --
--printlist (fold_left (cons) (nil) (list({1,2,3})))

-- Exercise 2.42 --
-- exercise left to reader to define appropriate functions
-- function queens(board_size)
--    local function queen_cols(k)
--       if (k == 0) then
--          return list({empty_board})
--       else
--          return (
--             filter (
--                function(positions) return isSafe(k, positions) end) (
--                flatmap (
--                   function(rest_of_queens)
--                      return
--                         map (
--                            function(new_row) return adjoin_position(new_row, k, rest_of_queens) end) (
--                            enumerate_interval(1, board_size))
--                    end) (
--                    queen_cols(k-1))))
--       end
--    end
--    return queen_cols(board_size)
-- end

-- Exercise 2.43 --
-- exercise left to reader to define appropriate functions
-- function queens(board_size)
--    local function queen_cols(k)
--       if (k == 0) then
--          return list({empty_board})
--       else
--          return (
--             filter (
--                function(positions) return isSafe(k, positions) end) (
--                flatmap (
--                   function(new_row)
--                      return
--                         map (
--                            function(rest_of_queens) return adjoin_position(new_row, k, rest_of_queens) end) (
--                            queen_cols(k-1))
--                    end) (
--                    enumerate_interval(1, board_size))))
--       end
--    end
--    return queen_cols(board_size)
-- end

-- 2.2.4 Hierarchical Data and the Closure Property - Example: a picture language

-- these two routines are to be written --
function draw_line(x, y) end
function wave(xframe) return xframe end

function Vect(init_x, init_y)
   local a = {}
   a.x = init_x
   a.y = init_y
   return a
end

function make_vect(x, y) return Vect(x, y) end
function xcor_vect(v) return v.x end
function ycor_vect(v) return v.y end
function add_vect(v1, v2)
    return make_vect(xcor_vect(v1) + xcor_vect(v2), ycor_vect(v1) + ycor_vect(v2))
end
function sub_vect(v1, v2)
   return make_vect(xcor_vect(v1) - xcor_vect(v2), ycor_vect(v1) - ycor_vect(v2))
end
function scale_vect(s, v)
   return make_vect(s * xcor_vect(v), s * ycor_vect(v))
end

function Frame(init_orig, init_edge1, init_edge2)
   local a = {}
   a.orig = init_orig
   a.edge1 = init_edge1
   a.edge2 = init_edge2
   return a
end

function make_frame(origin, edge1, edge2)
   return Frame(origin, edge1, edge2)
end
function origin_frame(f) return f.orig end
function edge1_frame(f) return f.edge1 end
function edge2_frame(f) return f.edge2 end
a_frame = make_frame(make_vect(0.0, 0.0), make_vect(1.0, 0.0), make_vect(0.0, 1.0))

function Segment(init_x, init_y)
   local a = {}
   a.x = init_x
   a.y = init_y
   return a
end

function start_segment(seg) return seg.x end
function end_segment(seg) return seg.y end

-- Frames --
function frame_coord_map(xframe, v)
   return add_vect(
      origin_frame(xframe),
      add_vect(scale_vect(xcor_vect(v), edge1_frame(xframe)),
               scale_vect(ycor_vect(v), edge2_frame(xframe))))
end

frame_coord_map(a_frame, make_vect(0.0, 0.0))
origin_frame(a_frame)

-- Painters --
function foreach(f)
   local function foreach_lambda(xs)
      if (x ~= nil) then
         f(car(xs))
         foreach (f) (cdr(xs))
      end
   end
   return foreach_lambda
end

function segments_painter(segment_list, xframe)
   foreach (
      function(segment)
         draw_line(
             frame_coord_map (xframe) (start_segment, segment),
             frame_coord_map (xframe) (end_segment, segment))
      end) (
      segment_list)
end

function transform_painter(painter, origin, corner1, corner2)
   local function transform_painter_lambda(xframe)
      local m = frame_coord_map(xframe)
      local new_origin = m(origin)
      return painter(
         make_frame(
            new_origin,
            sub_vect(m(corner1), new_origin),
            sub_vect(m(corner2), new_origin)))
   end
   return transform_painter_lambda
end

function flip_vert(painter)
   return transform_painter(
      painter,
      make_vect(0.0, 1.0),
      make_vect(1.0, 1.0),
      make_vect(0.0, 0.0))
end

function flip_horiz(painter)
   return transform_painter(
      painter,
      make_vect(1.0, 0.0),
      make_vect(0.0, 0.0),
      make_vect(1.0, 1.0))
end

function shrink_to_upper_right(painter)
   return transform_painter(
      painter,
      make_vect(0.5, 0.5),
      make_vect(1.0, 0.5),
      make_vect(0.5, 1.0))
end

function rotate90(painter)
   return transform_painter(
      painter,
      make_vect(1.0, 0.0),
      make_vect(1.0, 1.0),
      make_vect(0.0, 0.0))
end

function rotate180(painter)
   return transform_painter(
      painter,
      make_vect(1.0, 1.0),
      make_vect(0.0, 1.0),
      make_vect(1.0, 0.0))
end

function squash_inwards(painter)
   return transform_painter(
      painter,
      make_vect(0.0, 0.0),
      make_vect(0.65, 0.35),
      make_vect(0.35, 0.65))
end

function beside(painter1, painter2)
   local function beside_lambda(xframe)
      local split_point = make_vect(0.5, 0.0)
      local paint_left = (
         transform_painter(
            painter1,
            make_vect(0.0, 0.0),
            split_point,
            make_vect(0.0, 1.0)))
      local paint_right = (
         transform_painter(
            painter2,
            split_point,
            make_vect(1.0, 0.0),
            make_vect(0.5, 1.0)))
      paint_left(xframe)
      paint_right(xframe)
   end
   return beside_lambda
end

function below(painter1, painter2)
   local function below_lambda(xframe)
      local split_point = make_vect(0.0, 0.5)
      local paint_below = (
         transform_painter(
            painter1,
            make_vect(0.0, 0.0),
            make_vect(1.0, 0.0),
            split_point))
      local paint_above = (
         transform_painter(
            painter2,
            split_point,
            make_vect(1.0, 0.5),
            make_vect(0.0, 1.0)))
      paint_below(xframe)
      paint_above(xframe)
   end
   return below_lambda
end

function up_split(painter, n)
   if (n == 0) then
      return painter
   else
      local smaller = up_split(painter, n-1)
      return below(painter, beside(smaller, smaller))
   end
end

wave2 = beside(wave, flip_vert(wave))

wave4 = below(wave2, wave)

function flipped_pairs(painter)
   local painter2 = beside(painter, flip_vert(painter))
   return below(painter2, painter2)
end

wave4 = flipped_pairs(wave)

function right_split(painter, n)
   if (n == 0) then
      return painter
   else
      local smaller = right_split(painter, n-1)
      return beside(painter, below(smaller, smaller))
   end
end

function corner_split(painter, n)
   if (n == 0) then
      return painter
   else
      local up = up_split(painter, n-1)
      local right = right_split(painter, n-1)
      local top_left = beside(up, up)
      local bottom_right = below(right, right)
      local corner = corner_split(painter, n-1)
      return beside(below(painter, top_left),  below(bottom_right, corner))
   end
end

function square_limit(painter, n)
   local quarter = corner_split(painter, n)
   local half = beside(flip_horiz(quarter), quarter)
   return below(flip_vert(half), half)
end

-- Higher_order operations --
function square_of_four(tleft, tright, bleft, bright)
   local function square_of_four_lambda(painter)
      local top = beside(tleft(painter), tright(painter))
      local bottom = beside(bright(painter), bright(painter))
      return below(bottom, top)
   end
   return square_of_four_lambda
end

function flipped_pairs(painter)
   local combine4 = square_of_four(identity, flip_vert, identity, flip_vert)
   return combine4(painter)
end

-- footnote --
flipped_pairs = square_of_four(identity, flip_vert, identity, flip_vert)

function square_limit(painter, n)
   local combine4 = square_of_four(flip_horiz, identity, rotate180, flip_vert)
   return combine4(corner_split(painter, n))
end

-- Exercise 2.45 --
-- exercise left to reader to define appropriate functions
-- right_split = split(beside, below)
-- up_split = split(below, beside)

-- Exercise 2.47 --
function make_frame(origin, edge1, edge2)
   return list({origin, edge1, edge2})
end
function make_frame(origin, edge1, edge2)
   return list({origin, list({edge1, edge2})})
end
