(* SICP Chapter #02 Examples in Alice ML / Standard ML *)
(* Functions defined in previous chapters *)
fun gcd (a, 0) = a
  | gcd (a, b) = gcd(b, a mod b)

fun fib 0 = 0
  | fib 1 = 1
  | fib n = fib(n - 1) + fib(n - 2)

fun identity x = x

(* 2 Building Abstractions with Data *)
fun linear_combination (a, b, x, y) =
   a*x + b*y

fun mul (a, b) = a * b
fun linear_combination (a, b, x, y) =
   mul(a, x) + mul(b, y)

(* 2.1.1 Introduction to Data Abstraction - Example: Arithmetic Operations for Rational Numbers *)

(* Literal Translation *)
   fun make_rat (n, d) = [n, d]
   fun numer x = hd x
   fun denom x = hd(tl x)

   fun add_rat (x, y) =
      make_rat((numer x * denom y) + (numer y * denom x), denom x * denom y)
   fun sub_rat (x, y) =
      make_rat((numer x * denom y) - (numer y * denom x), denom x * denom y)
   fun mul_rat (x, y) =
      make_rat(numer x * numer y, denom x * denom y)
   fun div_rat (x, y) =
      make_rat(numer x * denom y, denom x * numer y)
   fun equal_rat (x, y) =
      ((numer x * denom y) = (numer y * denom x))

   fun cons (x, y) = [x, y]
   val car = hd
   val cdr = tl
   (* Alice ML version *)
   val cadr = car o cdr
   (* end Alice ML version *)
   fun cadr x = car(cdr x)

   val x = cons(1, 2);
   car x;
   cdr x;

   val x = cons(1, 2)
   val y = cons(3, 4)
   val z = cons(x, y);
   car(car z);
   car(cdr z);

   (* footnote -- alternative definitions *)
   val make_rat = cons
   val numer = car
   (* Alice ML version *)
   val denom = car o cdr
   (* end Alice ML version *)

   fun print_rat x =
      let in
         print "\n";
         print (Int.toString(numer x));
         print "/";
         print (Int.toString(denom x))
      end

   val one_half = make_rat(1, 2);
   print_rat one_half;

   val one_third = make_rat(1, 3);
   print_rat(add_rat(one_half, one_third));
   print_rat(mul_rat(one_half, one_third));
   print_rat(add_rat(one_third, one_third));

   (* reducing to lowest terms in constructor *)
   fun make_rat (n, d) =
      let
         val g = gcd(n, d)
      in
         [n div g, d div g]
      end

   fun add_rat (x, y) =
      make_rat((numer x * denom y) + (numer y * denom x), denom x * denom y);

   print_rat(add_rat(one_third, one_third));
(* end Literal Translation *)

(* Module Translation *)
   signature RATIONAL =
      sig
         type rational
         val make_rat  : int * int -> rational
         val numer     : rational -> int
         val denom     : rational -> int
         val add_rat   : rational * rational -> rational
         val sub_rat   : rational * rational -> rational
         val mul_rat   : rational * rational -> rational
         val div_rat   : rational * rational -> rational
         val equal_rat : rational * rational -> bool
         val print_rat : rational -> unit
      end

   structure Rational :> RATIONAL =
      struct
         type rational = int list
         fun make_rat (n, d) = [n, d]
         fun numer x = hd x
         fun denom x = hd(tl x)
         fun add_rat (x, y) =
            make_rat((numer x * denom y) + (numer y * denom x), denom x * denom y)
         fun sub_rat (x, y) =
            make_rat((numer x * denom y) - (numer y * denom x), denom x * denom y)
         fun mul_rat (x, y) =
            make_rat(numer x * numer y, denom x * denom y)
         fun div_rat (x, y) =
            make_rat(numer x * denom y, denom x * numer y)
         fun equal_rat (x, y) =
            ((numer x * denom y) = (numer y * denom x))
         fun print_rat x = print("\n" ^ Int.toString(numer x) ^ "/" ^ Int.toString(denom x))
      end

   val one_half = Rational.make_rat(1, 2);
   Rational.print_rat one_half;

   val one_third = Rational.make_rat(1, 3);
   Rational.print_rat(Rational.add_rat(one_half, one_third));
   Rational.print_rat(Rational.mul_rat(one_half, one_third));
   Rational.print_rat(Rational.add_rat(one_third, one_third));

   (* reducing to lowest terms in constructor *)
   structure Rational :> RATIONAL =
      struct
         type rational = int list
         fun make_rat (n, d) =
            let
               val g = gcd(n, d)
            in
               [n div g, d div g]
            end
         fun numer x = hd x
         fun denom x = hd(tl x)
         fun add_rat (x, y) =
            make_rat((numer x * denom y) + (numer y * denom x), denom x * denom y)
         fun sub_rat (x, y) =
            make_rat((numer x * denom y) - (numer y * denom x), denom x * denom y)
         fun mul_rat (x, y) =
            make_rat(numer x * numer y, denom x * denom y)
         fun div_rat (x, y) =
            make_rat(numer x * denom y, denom x * numer y)
         fun equal_rat (x, y) =
            ((numer x * denom y) = (numer y * denom x))
         fun print_rat x = print("\n" ^ Int.toString(numer x) ^ "/" ^ Int.toString(denom x))
      end

   val one_third = Rational.make_rat(1, 3);
   Rational.print_rat(Rational.add_rat(one_third, one_third));
(* end Module Translation *)

(* Exercise 2.1 *)
fun make_rat (n, d) =
   if ((d < 0 andalso n < 0) orelse n < 0)
      then [d * ~1, n * ~1]
      else [d, n]

(* 2.1.2 Introduction to Data Abstraction - Abstraction barriers *)

(* Literal Translation *)
   (* reducing to lowest terms in selectors *)
   fun make_rat (n, d) = cons(n, d)

   fun numer x =
      let
         val g = gcd(car x, cadr x)
      in
         car x div g
      end

   fun denom x =
      let
         val g = gcd(car x, cadr x)
      in
         cadr x div g
      end
(* end Literal Translation *)

(* Module Translation *)
   (* reducing to lowest terms in selectors *)
   structure Rational :> RATIONAL =
      struct
         type rational = int list
         fun make_rat (n, d) = [n, d]
         fun numer x =
            let
               val n = hd x
               val d = hd(tl x)
            in
               n div gcd(n, d)
            end
         fun denom x =
            let
               val n = hd x
               val d = hd(tl x)
            in
               d div gcd(n, d)
            end
         fun add_rat (x, y) =
            make_rat((numer x * denom y) + (numer y * denom x), denom x * denom y)
         fun sub_rat (x, y) =
            make_rat((numer x * denom y) - (numer y * denom x), denom x * denom y)
         fun mul_rat (x, y) =
            make_rat(numer x * numer y, denom x * denom y)
         fun div_rat (x, y) =
            make_rat(numer x * denom y, denom x * numer y)
         fun equal_rat (x, y) =
            ((numer x * denom y) = (numer y * denom x))
         fun print_rat x = print("\n" ^ Int.toString(numer x) ^ "/" ^ Int.toString(denom x))
      end
(* end Module Translation *)

(* Exercise 2.2 *)
fun make_point (x, y) = [x, y]
fun x_point point = hd point
fun y_point point = hd(tl point)
fun make_segment (start_segment, end_segment) = [start_segment, end_segment]
fun start_segment segment = hd segment
fun end_segment segment = hd(tl segment)
fun midpoint_segment segment =
   let
      val s = start_segment segment
      val e = end_segment segment
   in
      make_point(((x_point s) + (x_point e)) / 2.0, ((y_point s) + (y_point e)) / 2.0)
   end
fun print_point p =
   let in
      print "\n";
      print "(";
      print (Real.toString(x_point p));
      print ",";
      print (Real.toString(y_point p));
      print ")"
   end

(* Exercise 2.3 *)
fun square_real x:real = x * x
fun length_segment segment =
   let
      val s = start_segment segment
      val e = end_segment segment
   in
      Math.sqrt(square_real((x_point e) - (x_point s)) + square_real((y_point e) - (y_point s)))
   end

(* Constructors create type tagged using <pair> *)
datatype 'a rectangle = Axy of { anchor:'a, xlen:'a, ylen:'a }
                      | Seg of { start_segment:'a, end_segment:'a }
fun make_rectangle (anchor, xlen, ylen) =
   Axy{anchor=anchor, xlen=xlen, ylen=ylen}

fun make_rectangle_2 (start_segment, end_segment) =
   Seg{start_segment=start_segment, end_segment=end_segment}

(* 'length_rectangle' and 'width_rectangle' act as an abstraction barrier for higher-level
   procedures because 'rectangle' implementation details are buried here, and should the
   implementation change, only these procedures will need to be altered to support the change *)
fun length_rectangle (Axy{anchor, xlen, ylen}) = 0.0                 (* Compute length ... *)
  | length_rectangle (Seg{start_segment, end_segment}) = 0.0         (* Compute length ... *)

fun width_rectangle rect =
   (* As per 'length_rectangle' except that rectangle width is returned ... *)
   0.0

(* High-level procedures are quarantined from representation / implementation details *)
fun area_rectangle rect =
   (length_rectangle rect) * (width_rectangle rect)

fun perimeter_rectangle rect =
   (length_rectangle rect) * 2.0 + (width_rectangle rect) * 2.0

(* 2.1.3 Introduction to Data Abstraction - What is meant by data? *)
datatype dispatch = Car | Cdr
datatype ('a,'b) pair' = Left of 'a | Right of 'b

fun cons (x, y) =
   let
      fun dispatch Car = Left x
        | dispatch Cdr = Right y
   in
      dispatch
   end

fun car z =
   case z Car of
      Left c => c
   | _ => raise Domain
fun cdr z =
   case z Cdr of
      Right c => c
   | _ => raise Domain

(* Exercise 2.4 *)
fun cons (x, y) =
   fn m => m(x, y)
fun car z =
   z(fn (p, q) => p)
fun cdr z =
   z(fn (p, q) => q)

(* Exercise 2.5 *)
fun power (x, 0) = 1
  | power (x, 1) = x
  | power (x, k) = x * power(x, k-1)
fun cons (x, y) =
  power(2, x * power(3, y))
fun car z =
   if (z mod 2) = 0
      then car((z div 2) + 1)
      else 0
fun cdr z =
   if (z mod 3) = 0
      then cdr((z div 3) + 1)
      else 0

(* Exercise 2.6 *)
fun zero f x = x
fun add1 n f x = f (n f x)

(* 2.1.4 Introduction to Data Abstraction - Extended Exercise: Interval Arithmetic *)

(* Literal Translation *)
   fun make_interval (a, b) = (a, b) : real*real

   fun lower_bound (x, y) = x
   fun upper_bound (x, y) = y

   fun add_interval (x, y) =
      make_interval(lower_bound x + lower_bound y, upper_bound x + upper_bound y)

   fun mul_interval (x, y) =
      let
         val p1 = lower_bound x * lower_bound y
         val p2 = lower_bound x * upper_bound y
         val p3 = upper_bound x * lower_bound y
         val p4 = upper_bound x * upper_bound y
      in
         make_interval(
            Real.min(Real.min(p1, p2), Real.min(p3, p4)),
            Real.max(Real.max(p1, p2), Real.max(p3, p4)))
      end

   fun div_interval (x, y) =
      let
         val z = make_interval(1.0 / upper_bound y, 1.0 / lower_bound y)
      in
         mul_interval(x, z)
      end

   fun make_center_width (c, w) =
      make_interval(c-w, c+w)

   fun center i =
      (lower_bound i + upper_bound i) / 2.0

   fun width i =
      (upper_bound i - lower_bound i) / 2.0

   (* parallel resistors *)
   fun par1 (r1, r2) =
      div_interval(mul_interval(r1, r2), add_interval(r1, r2))

   fun par2 (r1, r2) =
      let
         val one = make_interval(1.0, 1.0)
      in
         div_interval(one,
            add_interval(div_interval(one, r1),
                         div_interval(one, r2)))
      end
(* end Literal Translation *)

(* Module Translation *)
   signature INTERVAL =
      sig
         type interval
         val make_interval     : real * real -> interval
         val lower_bound       : interval -> real
         val upper_bound       : interval -> real
         val add_interval      : interval * interval -> interval
         val mul_interval      : interval * interval -> interval
         val div_interval      : interval * interval -> interval
         val make_center_width : real * real -> interval
         val center            : interval -> real
         val width             : interval -> real
      end

   structure Interval :> INTERVAL =
      struct
         type interval = real * real
         fun make_interval (a, b) = (a, b)
         fun lower_bound (x, y) = x : real
         fun upper_bound (x, y) = y : real
         fun add_interval (x, y) =
            make_interval(lower_bound x + lower_bound y, upper_bound x + upper_bound y)
         fun mul_interval (x, y) =
            let
               val p1 = lower_bound x * lower_bound y
               val p2 = lower_bound x * upper_bound y
               val p3 = upper_bound x * lower_bound y
               val p4 = upper_bound x * upper_bound y
            in
               make_interval(
                  Real.min(Real.min(p1, p2), Real.min(p3, p4)),
                  Real.max(Real.max(p1, p2), Real.max(p3, p4)))
            end
         fun div_interval (x, y) =
            let
               val z = make_interval(1.0 / upper_bound y, 1.0 / lower_bound y)
            in
               mul_interval(x, z)
            end
         fun make_center_width (c, w) : real * real =
            make_interval(c-w, c+w)
         fun center i =
            (lower_bound i + upper_bound i) / 2.0
         fun width i =
            (upper_bound i - lower_bound i) / 2.0
      end

   (* parallel resistors *)
   fun par1 (r1, r2) =
      Interval.div_interval(Interval.mul_interval(r1, r2), Interval.add_interval(r1, r2))

   fun par2 (r1, r2) =
      let
         val one = Interval.make_interval(1.0, 1.0)
      in
         Interval.div_interval(one,
            Interval.add_interval(Interval.div_interval(one, r1),
                                  Interval.div_interval(one, r2)))
      end
(* end Module Translation *)

(* Exercise 2.7 *)
fun make_interval (a, b) = (a, b)
fun lower_bound (x, y) = x
fun upper_bound (x, y) = y

(* Exercise 2.8 *)
fun sub_interval (x:real*real, y:real*real) =
   make_interval(lower_bound x - lower_bound y, upper_bound x - upper_bound y)

(* Exercise 2.9 *)
val i = make_interval(5.0, 10.0)
val j = make_interval(15.0, 25.0)

(* width of the sum (or difference) of two intervals *is* a function only of the widths of
   the intervals being added (or subtracted) *)
val _ = (width(add_interval(i, j)), width i + width j)
val _ = (width(sub_interval(i, j)), width i + width j)

(* width of the product (or quotient) of two intervals *is not* a function only of the widths
   of the intervals being multiplied (or divided) *)
val _ = (width(mul_interval(i, j)), width i + width j)
val _ = (width(div_interval(i, j)), width i + width j)

(* Exercise 2.10 *)
exception DivideByZero of string;
fun is_zero_interval i =
   Real.==(lower_bound i, 0.0) orelse Real.==(upper_bound i, 0.0)
fun div_interval_zero_check (x, y) =
   if is_zero_interval y
      then raise DivideByZero("Zero interval divisor")
      else div_interval(x, y)

(* Exercise 2.12 *)
fun make_center_percent (c, p) =
   make_center_width(c, p * c / 100.0)
fun percent i =
   width i / (center i) * 100.0

(* 2.2.1 Hierarchical Data and the Closure Property - Representing Sequences *)

val _ = 1::2::3::4::nil;
val one_through_four = [1, 2, 3, 4];

one_through_four;
hd one_through_four;
tl one_through_four;
hd (tl one_through_four);
10::one_through_four;

fun list_ref (items, 0) = hd items
  | list_ref (items, n) = list_ref(tl items, n-1)

val squares = [1, 4, 9, 16, 25];
list_ref(squares, 3);

fun length nil = 0
  | length items = 1 + length(tl items)

val odds = [1, 3, 5, 7];
length odds;

fun length items =
   let
      fun length_iter (nil, count) = count
        | length_iter (a, count) = length_iter(tl a, 1+count)
   in
      length_iter(items, 0)
   end

fun append (nil, list2)   = list2
  | append (list1, list2) =
      hd list1 :: append(tl list1, list2);

append(squares, odds);
append(odds, squares);

(* Mapping over lists *)
fun scale_list(factor, nil) = nil
  | scale_list(factor, x::xs) =
      x * factor :: scale_list(factor, xs);

scale_list(10, [1, 2, 3, 4, 5]);

fun map proc nil = nil
  | map proc items = proc(hd items) :: map proc (tl items);

map abs [~10.0, 2.5, ~11.6, 17.0];

map (fn x => x * x) [1, 2, 3, 4];

fun scale_list(factor, items) =
   map (fn x => x * factor) items

(* Not sure how to translate these to ML?
   (map + (list 1 2 3) (list 40 50 60) (list 700 800 900))
   (map (lambda (x y) (+ x ( * 2 y))) (list 1 2 3) (list 4 5 6)) *)

(* Exercise 2.17 *)
fun last_pair xs =
   let
      fun last_pair_iter (nil, prev)   = prev
        | last_pair_iter (x::xs, prev) = last_pair_iter(xs, [x])
   in
      last_pair_iter(xs, nil)
   end;
last_pair [23, 72, 149, 34];

(* Exercise 2.18 *)
fun reverse nil = nil
  | reverse (x::xs) = append(reverse xs, [x]);
reverse [1, 4, 9, 16, 25];
fun reverse xs =
   let
      fun reverse_iter (nil, accum) = accum
        | reverse_iter (x::xs, accum) = reverse_iter(xs, x::accum)
   in
      reverse_iter(xs, nil)
   end;
reverse [1, 4, 9, 16, 25];

(* Exercise 2.19 *)
fun no_more nil = true
  | no_more coin_values = false
fun except_first_denomination coin_values = tl coin_values
fun first_denomination coin_values = hd coin_values
fun cc (amount, coin_values) =
   if amount = 0
      then 1
      else
         if amount < 0 orelse no_more coin_values
            then 0
            else
               cc(amount, except_first_denomination coin_values) +
               cc(amount - (first_denomination coin_values), coin_values)
val us_coins = [50, 25, 10, 5, 1];
cc(100, us_coins);
(* Note: ML doesn't like mixing ints and floats (answer should be 104561) *)
(* val uk_coins = [100, 50, 20, 10, 5, 2, 1, 0.5]; *)
(* cc(100, uk_coins); *)

(* Exercise 2.20 *)
fun filter (predicate, nil) = nil
  | filter (predicate, x::xs) =
      if predicate x
         then x::filter(predicate, xs)
         else filter(predicate, xs)
fun isOdd n = (n mod 2 = 1)
val isEven = not o isOdd
fun same_parity xs =
   let
      val predicate = if isOdd (hd xs) then isOdd else isEven
   in
      filter(predicate, tl xs)
   end;
same_parity [1, 2, 3, 4, 5, 6, 7];
same_parity [2, 3, 4, 5, 6, 7];

(* Exercise 2.21 *)
fun square_list nil     = nil
  | square_list (x::xs) = (x*x)::square_list xs;
square_list [1, 2, 3, 4];
fun square_list xs =
   map (fn x => x*x) xs;
square_list [1, 2, 3, 4];

(* Exercise 2.22 *)
fun square x = x * x
fun square_list items =
   let
      fun iter (nil, answer)   = answer
        | iter (x::xs, answer) = iter(xs, square x::answer)
   in
      iter(items, nil)
   end;
square_list [1, 2, 3, 4];
fun square_list items =
   let
      fun iter (nil, answer)   = answer
        | iter (x::xs, answer) = iter(xs, answer@[square x])
   in
      iter(items, nil)
   end;
square_list [1, 2, 3, 4];
fun square_list items =
   let
      fun iter (nil, answer)   = answer
        | iter (x::xs, answer) = iter(xs, square x::answer)
   in
      reverse(iter(items, nil))
   end;
square_list [1, 2, 3, 4];

(* Exercise 2.23 *)
fun for_each nil f = ()
  | for_each (x::xs) f = (f x; for_each xs f);
for_each [57, 321, 88] (fn x => (print "\n"; print (Int.toString x)));

(* 2.2.2 Hierarchical Data and the Closure Property - Hierarchical Structures *)
datatype 'a nestedlist = Leaf of 'a
                       | Node of 'a nestedlist list

fun length' (Node x) = length x
  | length' (Leaf x) = 1;

Node[Node[Leaf 1, Leaf 2], Node[Leaf 3, Leaf 4]];
val x = Node[Node[Leaf 1, Leaf 2], Node[Leaf 3, Leaf 4]];
length' x;

fun count_leaves (Node nil) = 0
  | count_leaves (Node x)   = count_leaves(hd x) + count_leaves(Node (tl x))
  | count_leaves (Leaf x)   = 1

val x = Node[Node[Leaf 1, Leaf 2], Node[Leaf 3, Leaf 4]];
length' x;
count_leaves x;

Node[x, x];
length'(Node[x, x]);
count_leaves(Node[x, x]);

(* Mapping over trees *)
fun scale_tree (factor, Leaf x)       = Leaf (x * factor)
  | scale_tree (factor, Node nil)     = Node nil
  | scale_tree (factor, Node (x::xs)) =
      let
         val a = scale_tree(factor, Node xs)
         val b = case a of
             Node c => c
           | Leaf c => [Leaf c]
      in
         Node (scale_tree(factor, x) :: b)
      end;

scale_tree(10, Node[Leaf 1, Node[Leaf 2, Node[Leaf 3, Leaf 4], Leaf 5], Node[Leaf 6, Leaf 7]]);

fun scale_tree (factor, Leaf x) = Leaf (x * factor)
  | scale_tree (factor, Node x) =
      Node(map (fn a => scale_tree(factor, a)) x);

(* Exercise 2.24 *)
Node[Leaf 1, Node[Leaf 2, Node[Leaf 3, Leaf 4]]];

(* Exercise 2.25 *)
Node[Leaf 1, Leaf 3, Node[Leaf 5, Leaf 7], Leaf 9];
Node[Node[Leaf 7]];
Node[Leaf 1, Node[Leaf 2, Node[Leaf 3, Node[Leaf 4, Node[Leaf 5, Node[Leaf 6, Leaf 7]]]]]];

(* Exercise 2.26 *)
val x = Node[Leaf 1, Leaf 2, Leaf 3]
val y = Node[Leaf 4, Leaf 5, Leaf 6]
fun append_tree(Node x, Leaf y) = Node (x @ [Leaf y])
  | append_tree(Leaf x, Node y) = Node (Leaf x::y)
  | append_tree(Node x, Node y) = Node (x@y)
  | append_tree(Leaf x, Leaf y) = Node [Leaf x, Leaf y];
append_tree(x, y);
Node[x, Node[y]];
Node[x, y];

(* Exercise 2.27 *)
val x = Node[Node[Leaf 1, Leaf 2], Node[Leaf 3, Leaf 4]]
fun reverse (Leaf x)  = Leaf x
  | reverse (Node xs) = Node (List.rev xs);
reverse x;
fun deep_reverse (Leaf x)  = Leaf x
  | deep_reverse (Node xs) = Node (rev (map deep_reverse xs));
deep_reverse x;

(* Exercise 2.28 *)
val x = Node[Node[Leaf 1, Leaf 2], Node[Leaf 3, Leaf 4]]
fun fringe (Leaf x)       = [x]
  | fringe (Node nil)     = nil
  | fringe (Node (x::xs)) = fringe x @ fringe (Node xs);
fringe x;
fringe(Node[x, x]);

(* Exercise 2.29 *)
(* a. *)
fun make_mobile (left, right) = Node[left, right]
fun make_branch (length, struc) = Node[Leaf length, struc]
fun left_branch (Node[left, right]) = left
  | left_branch _ = raise Domain
fun right_branch (Node[left, right]) = right
  | right_branch _ = raise Domain
fun branch_length (Node[Leaf length, struc]) = length
  | branch_length _ = raise Domain
fun branch_structure (Node[Leaf length, struc]) = struc
  | branch_structure _ = raise Domain
(* Remainder To Be Done *)

(* Exercise 2.30 *)
fun square_tree (Leaf x)  = Leaf (x*x)
  | square_tree (Node xs) = Node(map square_tree xs);
square_tree(
   Node[Leaf 1,
     Node[Leaf 2, Node[Leaf 3, Leaf 4], Leaf 5],
     Node[Leaf 6, Leaf 7]]);

(* Exercise 2.31 *)
fun tree_map proc (Leaf x)  = Leaf(proc x)
  | tree_map proc (Node xs) = Node(map (tree_map proc) xs)
fun square_tree tree = tree_map square tree;
square_tree(
   Node[Leaf 1,
     Node[Leaf 2, Node[Leaf 3, Leaf 4], Leaf 5],
     Node[Leaf 6, Leaf 7]]);

(* Exercise 2.32 *)
fun subsets nil = [nil]
  | subsets (x::xs) =
      let
         val rest = subsets xs
      in
         rest @ map (fn y => x::y) rest
      end;
subsets [1, 2, 3];

(* 2.2.3 Hierarchical Data and the Closure Property - Sequences as Conventional Interfaces *)
fun isOdd n = (n mod 2 = 1)
val isEven = not o isOdd
fun square x = x * x

fun sum_odd_squares (Node nil) = 0
  | sum_odd_squares (Leaf x) =
      if isOdd x
         then square x
         else 0
  | sum_odd_squares (Node (x::xs)) =
      sum_odd_squares(x) + sum_odd_squares(Node xs);

fun even_fibs n =
   let
      fun next k =
         if k > n
            then nil
            else
               let
                  val f = fib k
               in
                  if isEven f
                     then f::next(k+1)
                     else next(k+1)
               end
   in
      next 0
   end;

(* Sequence operations *)
map square [1,2,3,4,5];

fun filter predicate nil = nil
  | filter predicate (sequence as x::xs) =
      if predicate x
         then x :: filter predicate xs
         else filter predicate xs;

filter isOdd [1,2,3,4,5];

fun accumulate oper initial nil = initial
  | accumulate oper initial (sequence as x::xs) =
      oper(x, accumulate oper initial xs);

accumulate op+ 0 [1,2,3,4,5];
accumulate op* 1 [1,2,3,4,5];
accumulate op:: nil [1,2,3,4,5];

fun enumerate_interval(low, high) =
   if low > high
      then nil
      else low::enumerate_interval(low+1, high);

enumerate_interval(2,7);

fun enumerate_tree (Node nil) = nil
  | enumerate_tree (Leaf x) = [x]
  | enumerate_tree (Node (x::xs)) =
      enumerate_tree(x) @ enumerate_tree(Node xs);

enumerate_tree(Node[Leaf 1, Node[Leaf 2, Node[Leaf 3, Leaf 4], Leaf 5]]);

fun sum_odd_squares tree =
   accumulate op+ 0 (map square (filter isOdd (enumerate_tree tree)))

fun even_fibs n =
   accumulate op:: nil (filter isEven (map fib (enumerate_interval(0, n))))

fun list_fib_squares n =
   accumulate op:: nil (map square (map fib (enumerate_interval(0, n))));

list_fib_squares 10;

fun product_of_squares_of_odd_elements sequence =
   accumulate op* 1 (map square (filter isOdd sequence));

product_of_squares_of_odd_elements [1,2,3,4,5];

datatype employee = Employee of { name:string, jobtitle:string, salary:int }
fun isProgrammer (Employee{jobtitle, ...}) = (jobtitle = "Programmer")
fun salary (Employee{salary, ...}) = salary
fun salary_of_highest_paid_programmer records =
   accumulate (Int.max) 0 (map salary (filter isProgrammer records));

val recs = [Employee{name="Fred", jobtitle="Programmer", salary=180},
            Employee{name="Hank", jobtitle="Programmer", salary=150}];
salary_of_highest_paid_programmer recs;

(* Nested mappings *)
val n = 10;                   (* book doesn't define n *)
accumulate op@ nil
   (map
      (fn i => map
         (fn j => [i, j])
         (enumerate_interval(1, i-1)))
      (enumerate_interval(1, n)));

fun flatmap proc seq =
   accumulate op@ nil (map proc seq)

fun has_no_divisors(n, 1) = true
  | has_no_divisors(n, c) =
      if n mod c = 0
         then false
         else has_no_divisors(n, c-1)

fun isPrime(n) = has_no_divisors(n, n-1)

fun prime_sum (x, y) = isPrime(x + y)

fun make_pair_sum (x, y) = (x, y, x+y)

fun prime_sum_pairs n =
   map make_pair_sum
      (filter
         prime_sum
         (flatmap
            (fn i => map
               (fn j => (i, j))
               (enumerate_interval(1, i-1)))
            (enumerate_interval(1, n))))

fun remove (item, sequence) =
   filter (fn x => x <> item) sequence

fun permutations nil = [nil]
  | permutations s =
      flatmap
         (fn x => map
            (fn p => x::p)
            (permutations (remove(x, s))))
         s

(* Exercise 2.34 *)
(* exercise left to reader to define appropriate functions
   fun horner_eval x coefficient_sequence =
      accumulate (fn (this_coeff, higher_terms) => ??FILL_THIS_IN??) 0 coefficient_sequence;
   horner_eval 2 [1,3,0,5,0,1]; *)

(* Exercise 2.36 *)
(* exercise left to reader to define appropriate functions
   fun accumulate_n oper init nil = nil
     | accumulate_n oper init seqs =
         (accumulate oper init ??FILL_THIS_IN??)::
            (accumulate_n oper init ??FILL_THIS_IN??);
   accumulate_n op+ 0 s; *)

(* CMR Error - need to finish this one *)
(* Exercise 2.37 *
fun dot_product(v, w) =
   accumulate
      op+
      0
      (map
            (fn i =>
               map
                  (fn j => i * j)
                  w)
            v)
*)

(* Exercise 2.38 *)
val fold_right = accumulate
fun fold_left oper initial sequence =
   let
      fun iter (result, nil)   = result
        | iter (result, x::xs) = iter(oper(result, x), xs)
   in
      iter(initial, sequence)
   end;
fold_right op/ 1.0 [1.0,2.0,3.0];
fold_left op/ 1.0 [1.0,2.0,3.0];
fold_right op:: nil [1,2,3];
(* CMR Error - won't compile - Scheme result = (((() 1) 2) 3) *)
(* fold_left op:: nil [1,2,3]; *)

(* Exercise 2.42 *)
(* exercise left to reader to define appropriate functions
   fun queens board_size =
      let
         fun queen_cols 0 = [empty_board]
           | queen_cols k =
               filter
                  (fn positions => isSafe(k, positions))
                  (flatmap
                     (fn rest_of_queens => map
                        (fn new_row => adjoin_position(new_row, k, rest_of_queens))
                        (enumerate_interval(1, board_size)))
                     (queen_cols(k-1)))
      in
         queen_cols board_size
      end *)

(* Exercise 2.43 *)
(* exercise left to reader to define appropriate functions
   fun queens board_size =
      let
         fun queen_cols 0 = [empty_board]
           | queen_cols k =
               filter
                  (fn positions => isSafe(k, positions))
                  (flatmap
                     (fn new_row => map
                        (fn rest_of_queens => adjoin_position(new_row, k, rest_of_queens))
                        (queen_cols(k-1)))
                     (enumerate_interval(1, board_size)))
      in
         queen_cols board_size
      end *)

(* 2.2.4 Hierarchical Data and the Closure Property - Example: a picture language *)

(* these two routines are to be written *)
fun draw_line (x, y) = ()
fun wave xframe = xframe

datatype vect = Vect of { x:real, y:real }
fun make_vect (x, y) = Vect{x=x, y=y}
fun xcor_vect (Vect{x, y}) = x
fun ycor_vect (Vect{x, y}) = y
fun add_vect (v1, v2) = make_vect(xcor_vect v1 + xcor_vect v2, ycor_vect v1 + ycor_vect v2)
fun sub_vect (v1, v2) = make_vect(xcor_vect v1 - xcor_vect v2, ycor_vect v1 - ycor_vect v2)
fun scale_vect (s, v) = make_vect(s * xcor_vect v, s * ycor_vect v)

datatype frame = Frame of { origin:vect, edge1:vect, edge2:vect }
fun make_frame (origin, edge1, edge2) = Frame{origin=origin, edge1=edge1, edge2=edge2}
fun origin_frame (Frame{origin, edge1, edge2}) = origin
fun edge1_frame (Frame{origin, edge1, edge2}) = edge1
fun edge2_frame (Frame{origin, edge1, edge2}) = edge2
val a_frame = make_frame(make_vect(0.0, 0.0), make_vect(1.0, 0.0), make_vect(0.0, 1.0))

datatype 'a segment = Segment of { x:'a, y:'a }
fun start_segment (Segment{x, y}) = x
fun end_segment (Segment{x, y}) = y

(* Frames *)
fun frame_coord_map xframe v =
    add_vect(
      origin_frame xframe,
      add_vect(scale_vect(xcor_vect v, edge1_frame xframe),
               scale_vect(ycor_vect v, edge2_frame xframe)))

val _ = frame_coord_map a_frame (make_vect(0.0, 0.0))
val _ = origin_frame a_frame

(* Painters *)
fun foreach f nil = ()
  | foreach f (x::xs) = (f x; foreach f xs);

fun segments_painter segment_list xframe =
   foreach
      (fn segment =>
         draw_line
            (frame_coord_map xframe (start_segment segment),
             frame_coord_map xframe (end_segment segment)))
      segment_list

fun transform_painter (painter, origin, corner1, corner2) xframe =
   let
      val m = frame_coord_map xframe
      val new_origin = m origin
   in
      painter(
         make_frame(
            new_origin,
            sub_vect(m corner1, new_origin),
            sub_vect(m corner2, new_origin)))
   end

fun flip_vert painter =
   transform_painter(
      painter,
      make_vect(0.0, 1.0),
      make_vect(1.0, 1.0),
      make_vect(0.0, 0.0))

fun flip_horiz painter =
   transform_painter(
      painter,
      make_vect(1.0, 0.0),
      make_vect(0.0, 0.0),
      make_vect(1.0, 1.0))

fun shrink_to_upper_right painter =
   transform_painter(
      painter,
      make_vect(0.5, 0.5),
      make_vect(1.0, 0.5),
      make_vect(0.5, 1.0))

fun rotate90 painter =
   transform_painter(
      painter,
      make_vect(1.0, 0.0),
      make_vect(1.0, 1.0),
      make_vect(0.0, 0.0))

fun rotate180 painter =
   transform_painter(
      painter,
      make_vect(1.0, 1.0),
      make_vect(0.0, 1.0),
      make_vect(1.0, 0.0))

fun squash_inwards painter =
   transform_painter(
      painter,
      make_vect(0.0, 0.0),
      make_vect(0.65, 0.35),
      make_vect(0.35, 0.65))

fun beside (painter1, painter2) xframe =
   let
      val split_point = make_vect(0.5, 0.0)
      val paint_left =
         transform_painter(
            painter1,
            make_vect(0.0, 0.0),
            split_point,
            make_vect(0.0, 1.0))
      val paint_right =
         transform_painter(
            painter2,
            split_point,
            make_vect(1.0, 0.0),
            make_vect(0.5, 1.0))
   in
      paint_left xframe;
      paint_right xframe
   end

fun below (painter1, painter2) xframe =
   let
      val split_point = make_vect(0.0, 0.5)
      val paint_below =
         transform_painter(
            painter1,
            make_vect(0.0, 0.0),
            make_vect(1.0, 0.0),
            split_point)
      val paint_above =
         transform_painter(
            painter2,
            split_point,
            make_vect(1.0, 0.5),
            make_vect(0.0, 1.0))
   in
      paint_below xframe;
      paint_above xframe
   end

fun up_split (painter, 0) = painter
  | up_split (painter, n) =
      let
         val smaller = up_split(painter, n-1)
      in
         below(painter, beside(smaller, smaller))
      end

val wave2 = beside(wave, flip_vert wave)

val wave4 = below(wave2, wave)

fun flipped_pairs painter =
   let
      val painter2 = beside(painter, flip_vert painter)
   in
      below(painter2, painter2)
   end

val wave4 = flipped_pairs wave

fun right_split (painter, 0) = painter
  | right_split (painter, n) =
      let
         val smaller = right_split(painter, n-1)
      in
         beside(painter, below(smaller, smaller))
      end

fun corner_split (painter, 0) = painter
  | corner_split (painter, n) =
      let
         val up = up_split(painter, n-1)
         val right = right_split(painter, n-1)
         val top_left = beside(up, up)
         val bottom_right = below(right, right)
         val corner = corner_split(painter, n-1)
      in
         beside(below(painter, top_left), below(bottom_right, corner))
      end

fun square_limit(painter, n) =
   let
      val quarter = corner_split(painter, n)
      val half = beside(flip_horiz quarter, quarter)
   in
      below(flip_vert half, half)
   end

(* Higher_order operations *)
fun square_of_four (tleft, tright, bleft, bright) =
   fn painter =>
      let
         val top = beside(tleft painter, tright painter)
         val bottom = beside(bleft painter, bright painter)
      in
         below(bottom, top)
      end

fun flipped_pairs painter =
   let
      val combine4 = square_of_four(identity, flip_vert, identity, flip_vert)
   in
      combine4 painter
   end

(* footnote *)
val  flipped_pairs = square_of_four(identity, flip_vert, identity, flip_vert)

fun square_limit (painter, n:int) =
   let
      val combine4 = square_of_four(flip_horiz, identity, rotate180, flip_vert)
   in
      combine4(corner_split(painter, n))
   end

(* Exercise 2.45 *)
(* exercise left to reader to define appropriate functions
   val right_split = split(beside, below)
   val up_split = split(below, beside) *)

(* Exercise 2.47 *)
fun make_frame (origin, edge1, edge2) = [origin, edge1, edge2]
fun make_frame (origin, edge1, edge2) = [origin, [edge1, edge2]]

(* 2.3.1 Symbolic Data - Quotation *)

(* To Be Done *)

(* 2.3.2 Symbolic Data - Example: Symbolic Differentiation *)

(* representing algebraic expressions *)
datatype term = Number of int
              | Variable of char
              | Sum of term * term
              | Product of term * term

fun isNumber (Number x) = true
  | isNumber _ = false

fun isSame_number (Number x, Number y) = (x = y)
  | isSame_number _ = false

fun isVariable (Variable x) = true
  | isVariable _ = false

fun isSame_variable (Variable x, Variable y) = (x = y)
  | isSame_variable _ = false

fun isSum (Sum (x, y)) = true
  | isSum _ = false

fun isProduct (Product (x, y)) = true
  | isProduct _ = false

fun make_sum (Number x, Number y) = Number (x + y)
  | make_sum (x, y) = Sum (x, y)

fun make_product (Number x, Number y) = Number (x * y)
  | make_product (x, y) = Product (x, y)

fun addend (Sum (x, y)) = x
  | addend _ = raise Domain

fun augend (Sum (x, y)) = y
  | augend _ = raise Domain

fun multiplier (Product (x, y)) = x
  | multiplier _ = raise Domain

fun multiplicand (Product (x, y)) = y
  | multiplicand _ = raise Domain

fun deriv (exp, var) =
   if isNumber exp
      then Number 0
      else
         if isVariable exp
            then
               if isSame_variable(exp, var)
                  then Number 1
                  else Number 0
            else
               if isSum exp
                  then make_sum(deriv(addend exp, var), deriv(augend exp, var))
                  else
                     if isProduct exp
                        then
                           make_sum(
                              make_product(multiplier exp, deriv(multiplicand exp, var)),
                              make_product(deriv(multiplier exp, var), multiplicand exp))
                        else raise Domain

(* dx(x + 3) = 1 *)
val d1 = deriv(Sum(Variable #"x", Number 3), Variable #"x")

(* dx(x*y) = y *)
val d2 = deriv(Product(Variable #"x", Variable #"y"), Variable #"x")

(* dx(x*y + x + 3) = y + 1 *)
val d3 = deriv(Sum(Sum(Product(Variable #"x", Variable #"y"), Variable #"x"), Number 3), Variable #"x")

(* With simplification *)
fun make_sum (Number 0, y) = y
  | make_sum (x, Number 0) = x
  | make_sum (Number x, Number y) = Number (x + y)
  | make_sum (x, y) = Sum (x, y)

fun make_product (Number 0, y) = Number 0
  | make_product (x, Number 0) = Number 0
  | make_product (Number 1, y) = y
  | make_product (x, Number 1) = x
  | make_product (Number x, Number y) = Number (x * y)
  | make_product (x, y) = Product (x, y)

fun deriv (Number x, var) = Number 0
  | deriv (Variable x, Variable y) =
      if x = y
         then Number 1
         else Number 0
  | deriv (Variable x, var) = Number 0
  | deriv (Sum (x, y), var) =
      make_sum(deriv(x, var), deriv(y, var))
  | deriv (Product(x, y), var) =
      make_sum(make_product(x, deriv(y, var)), make_product(deriv(x, var), y))

(* dx(x + 3) = 1 *)
val d1 = deriv(Sum(Variable #"x", Number 3), Variable #"x")

(* dx(x*y) = y *)
val d2 = deriv(Product(Variable #"x", Variable #"y"), Variable #"x")

(* dx(x*y + x + 3) = y + 1 *)
val d3 = deriv(Sum(Sum(Product(Variable #"x", Variable #"y"), Variable #"x"), Number 3), Variable #"x")

(* EXERCISE 2.57 *)
(* dx(x*y*(x+3)) = dx(x*x*y + x*y*3) = 2xy + 3y *)
(* exercise left to reader to define appropriate functions
   val d4 = derivx(Product(Product(Variable #"x", Variable #"y"), Sum(Variable #"x", Number 3)), Variable #"x") *)

(* 2.3.3 Symbolic Data - Example: Representing Sets *)

(* unordered *)
fun is_element_of_set (x, nil) = false
  | is_element_of_set (x, y::ys) =
      if x = y
         then true
         else is_element_of_set (x, ys)

fun adjoin_set (x, set) =
   if is_element_of_set(x, set)
      then set
      else x::set

fun intersection_set (nil, set2) = nil
  | intersection_set (set1, nil) = nil
  | intersection_set (x::xs, set2) =
      if is_element_of_set(x, set2)
         then x::intersection_set(xs, set2)
         else intersection_set(xs, set2)

(* ordered *)
fun is_element_of_set (x, nil) = false
  | is_element_of_set (x, y::ys) =
      if x = y
         then true
         else
            if x < y
               then false
               else is_element_of_set (x, ys)

fun intersection_set (nil, set2) = nil
  | intersection_set (set1, nil) = nil
  | intersection_set (set1 as x::xs, set2 as y::ys) =
      if x = y
         then x::intersection_set(xs,ys)
         else
            if x < y
               then intersection_set(xs, set2)
               else intersection_set(set1, ys)

(* binary trees *)
datatype 'a btree = Leaf
                  | Node of 'a * 'a btree * 'a btree

fun is_element_of_set (x, Leaf) = false
  | is_element_of_set (x, Node(y, left, right)) =
      if x = y
         then true
         else
            if x < y
               then is_element_of_set(x, left)
               else is_element_of_set(x, right)

val _ = is_element_of_set(3, Node(2, Node(1, Leaf, Leaf), Node(3, Leaf, Leaf)));

fun adjoin_set (x, Leaf) = Node(x, Leaf, Leaf)
  | adjoin_set (x, set as Node(y, left, right)) =
      if x = y
         then set
         else
            if x < y
               then Node(y, adjoin_set(x, left), right)
               else Node(y, left, adjoin_set(x, right))

val _ = adjoin_set(3, Node(4, Node(2, Leaf, Leaf), Node(6, Leaf, Leaf)));

(* Exercise 2.63 *)
fun tree_to_list_1 (Leaf) = nil
  | tree_to_list_1 (Node(y, left, right)) =
      tree_to_list_1(left) @ (y::tree_to_list_1(right))
val _ = tree_to_list_1(Node(4, Node(2, Leaf, Leaf), Node(6, Leaf, Leaf)));

fun tree_to_list_2 tree =
   let
      fun copy_to_list (Leaf, ys) = ys
        | copy_to_list (Node(x, left, right), ys) =
            copy_to_list(left, x::copy_to_list(right, ys))
   in
      copy_to_list(tree, nil)
   end
val _ = tree_to_list_2(Node(4, Node(2, Leaf, Leaf), Node(6, Leaf, Leaf)));

(* Exercise 2.64 *)
fun partial_tree (elts, 0) = (Leaf, elts)
  | partial_tree (elts, n) =
      let
         val left_size = (n - 1) div 2
         val right_size = n - (left_size + 1)
         val left_result as (left_tree, non_left_elts) = partial_tree(elts, left_size)
         val this_entry = hd non_left_elts
         val right_result as (right_tree, remaining_elts) = partial_tree(tl non_left_elts, right_size)
      in
         (Node(this_entry, left_tree, right_tree), remaining_elts)
      end

fun list_to_tree elements =
   let
      val (result, _) = partial_tree(elements, length elements)
   in
      result
   end

val _ = list_to_tree [2, 4, 6]

(* information retrieval *)
datatype information = Information of { key:int, name:string, age:int }
fun lookup (given_key, nil) = raise Domain
  | lookup (given_key, (x as Information{key=key, ...})::xs) =
      if given_key = key
         then x
         else lookup(given_key, xs)

(* 2.3.4 Symbolic Data - Example: Huffman Encoding Trees *)

(* representing *)
datatype ('a, 'b) btree = Leaf of { symbol:'a, weight:'b }
                        | Tree of { left:('a, 'b) btree, right:('a, 'b) btree, symbols:'a list, weight:'b }

fun make_leaf (symbol, weight) = Leaf{ symbol=symbol, weight=weight }

fun isLeaf (Leaf{ ... }) = true
  | isLeaf _ = false

fun symbol_leaf (Leaf{ symbol, ... }) = symbol
  | symbol_leaf _ = raise Domain

fun weight_leaf (Leaf{ weight, ... }) = weight
  | weight_leaf _ = raise Domain

fun symbols (Leaf{ symbol, ... }) = [symbol]
  | symbols (Tree{ symbols, ... }) = symbols

fun weight (Leaf{ weight, ... }) = weight
  | weight (Tree{ weight, ... }) = weight

fun make_code_tree (left, right) =
   Tree{ left = left, right = right, symbols = symbols left @ symbols right, weight = weight left + weight right }

fun left_Node (Tree{ left, ... }) = left
  | left_Node _ = raise Domain
fun right_Node (Tree{ right, ... }) = right
  | right_Node _ = raise Domain

fun choose_Node (0, node) = left_Node node
  | choose_Node (1, node) = right_Node node
  | choose_Node _ = raise Domain

(* decoding *)
fun decode (bits, tree) =
   let
      fun decode_1 (nil, current_Node) = nil
        | decode_1 (bits as x::xs, current_Node) =
            let
               val next_Node = choose_Node (x, current_Node)
            in
               if isLeaf next_Node
                  then symbol_leaf next_Node::decode_1(xs, tree)
                  else decode_1(xs, next_Node)
            end
   in
      decode_1(bits, tree)
   end

(* sets *)
fun adjoin_set (x, nil) = [x]
  | adjoin_set (x, set as y::ys) =
      if weight x < weight y
         then x::set
         else y::adjoin_set(x, ys)

fun make_leaf_set (Leaf{ symbol, weight }::pairs) =
      adjoin_set(make_leaf(symbol, weight), make_leaf_set pairs)
  | make_leaf_set _ = raise Domain

(* Exercise 2.67 *)
val sample_tree =
   make_code_tree(make_leaf(#"A", 4),
      make_code_tree(make_leaf(#"B", 2),
         make_code_tree(make_leaf(#"D", 1), make_leaf(#"C", 1))))

val sample_message = [0, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0]

val test = implode(decode(sample_message, sample_tree))

(* Exercise 2.68 *)
(* exercise left to reader to define appropriate functions
   fun encode (nil, tree) = nil
     | encode (message as x::xs, tree) =
         encode_symbol(x, tree) @ encode(xs, tree) *)

(* 2.4.1 Multiple Representations for Abstract Data - Representations for Complex Numbers *)

fun square_real x : real = x * x

(* Rectangular *)
fun real_part z = hd z
fun imag_part z = hd(tl z)

fun magnitude z =
  Math.sqrt(square_real(real_part z) + square_real(imag_part z))

fun angle z =
  Math.atan2(imag_part z, real_part z)

fun make_from_real_imag (x, y) = [x, y]
fun make_from_mag_ang (r, a) =
   [r * Math.cos a,  r * Math.sin a]

(* polar *)
fun magnitude z = hd z : real
fun angle z = hd(tl z)

fun real_part z =
   magnitude z * Math.cos(angle z)

fun imag_part z =
   magnitude z * Math.sin(angle z)

fun make_from_real_imag (x, y) =
   [Math.sqrt(square_real x + square_real y), Math.atan2(y, x)]

fun make_from_mag_ang (r, a) = [r, a]

(* using the abstract type *)
val z = [1.0, 2.0]
val _ = make_from_real_imag(real_part z, imag_part z)
val _ = make_from_mag_ang(magnitude z, angle z)

fun add_complex (z1, z2) =
   make_from_real_imag(real_part z1 + real_part z2,
                       imag_part z1 + imag_part z2)

fun sub_complex (z1, z2) =
   make_from_real_imag(real_part z1 - real_part z2,
                       imag_part z1 - imag_part z2)

fun mul_complex (z1, z2) =
   make_from_mag_ang(magnitude z1 * magnitude z2,
                     angle z1 + angle z2)

fun div_complex (z1, z2) =
   make_from_mag_ang(magnitude z1 / magnitude z2,
                     angle z1 - angle z2)

(* 2.4.2 Multiple Representations for Abstract Data - Tagged Data *)

(* Using List with Intersection type *)
   datatype 'a tag = Rectangular | Polar | Contents of 'a

   fun attach_tag (type_tag, contents) = type_tag::contents

   fun type_tag (Rectangular::_) = Rectangular
     | type_tag (Polar::_) = Polar
     | type_tag _ = raise Domain

   fun contents (_ ::Contents x::nil) = x
     | contents _ = raise Domain

   fun isRectangular (Rectangular::_) = true
     | isRectangular _ = false

   fun isPolar (Polar::_) = true
     | isPolar _ = false

   (* Rectangular *)
   fun make_from_real_imag_rectangular (x, y) =
      [Rectangular, Contents x, Contents y]
   fun make_from_mag_ang_rectangular (r, a) =
      [Rectangular, Contents (r * Math.cos a), Contents (r * Math.sin a)]

   fun real_part_rectangular (Rectangular::Contents x::_) = x
     | real_part_rectangular _ = raise Domain
   fun imag_part_rectangular (Rectangular::_::Contents y::_) = y
     | imag_part_rectangular _ = raise Domain

   fun magnitude_rectangular z =
      Math.sqrt(square_real(real_part_rectangular z) +
                square_real(imag_part_rectangular z))
   fun angle_rectangular z =
      Math.atan2(imag_part_rectangular z, real_part_rectangular z)

   (* Polar *)
   fun make_from_real_imag_polar (x, y) =
      [Polar, Contents (Math.sqrt(square_real x + square_real y)), Contents (Math.atan2(y, x))]
   fun make_from_mag_ang_polar (r, a) =
      [Polar, Contents r, Contents a]

   fun magnitude_polar (Polar::Contents x::_) = x
     | magnitude_polar _ = raise Domain
   fun angle_polar (Polar::_::Contents y::_) = y
     | angle_polar _ = raise Domain

   fun real_part_polar z =
      magnitude_polar z * Math.cos(angle_polar z)
   fun imag_part_polar z =
      magnitude_polar z * Math.sin(angle_polar z)

   (* Generic selectors *)
   fun real_part (z as Rectangular::_) = real_part_rectangular z
     | real_part (z as Polar::_) = real_part_polar z
     | real_part _ = raise Domain
   fun imag_part (z as Rectangular::_) = imag_part_rectangular z
     | imag_part (z as Polar::_) = imag_part_polar z
     | imag_part _ = raise Domain

   fun magnitude (z as Rectangular::_) = magnitude_rectangular z
     | magnitude (z as Polar::_) = magnitude_polar z
     | magnitude _ = raise Domain
   fun angle (z as Rectangular::_) = angle_rectangular z
     | angle (z as Polar::_) = angle_polar z
     | angle _ = raise Domain
(* End Using List with Intersection type *)

(* Using Records *)
   datatype 'a tag = Rectangular of { real_part : 'a, imag_part : 'a }
                   | Polar of { magnitude : 'a, angle : 'a }

   fun isRectangular (Rectangular{...}) = true
     | isRectangular _ = false

   fun isPolar (Polar{...}) = true
     | isPolar _ = false

   (* Rectangular *)
   fun make_from_real_imag_rectangular (x, y) =
      Rectangular{ real_part = x, imag_part = y }
   fun make_from_mag_ang_rectangular (r, a) =
      Rectangular{ real_part = r * Math.cos a, imag_part = r * Math.sin a }

   fun real_part_rectangular (Rectangular{ real_part=x, ... }) = x
     | real_part_rectangular _ = raise Domain
   fun imag_part_rectangular (Rectangular{ imag_part=y, ... }) = y
     | imag_part_rectangular _ = raise Domain

   fun magnitude_rectangular z =
      Math.sqrt(square_real(real_part_rectangular z) +
                square_real(imag_part_rectangular z))
   fun angle_rectangular z =
      Math.atan2(imag_part_rectangular z, real_part_rectangular z)

   (* Polar *)
   fun make_from_real_imag_polar (x, y) =
      Polar { magnitude = Math.sqrt(square_real x + square_real y), angle = Math.atan2(y, x) }
   fun make_from_mag_ang_polar (r, a) =
      Polar { magnitude = r, angle = a }

   fun magnitude_polar (Polar{ magnitude=x, ... }) = x
     | magnitude_polar _ = raise Domain
   fun angle_polar (Polar{ angle=y, ... }) = y
     | angle_polar _ = raise Domain

   fun real_part_polar z =
      magnitude_polar z * Math.cos(angle_polar z)
   fun imag_part_polar z =
      magnitude_polar z * Math.sin(angle_polar z)

   (* Generic selectors *)
   fun real_part (z as Rectangular{ ... }) = real_part_rectangular z
     | real_part (z as Polar{ ... }) = real_part_polar z
   fun imag_part (z as Rectangular{ ... }) = imag_part_rectangular z
     | imag_part (z as Polar{ ... }) = imag_part_polar z

   fun magnitude (z as Rectangular{ ... }) = magnitude_rectangular z
     | magnitude (z as Polar{ ... }) = magnitude_polar z
   fun angle (z as Rectangular{ ... }) = angle_rectangular z
     | angle (z as Polar{ ... }) = angle_polar z
(* End Using Records *)

(* same as before *)
fun add_complex (z1, z2) =
   make_from_real_imag(real_part z1 + real_part z2,
                       imag_part z1 + imag_part z2)
fun sub_complex (z1, z2) =
   make_from_real_imag(real_part z1 - real_part z2,
                       imag_part z1 - imag_part z2)
fun mul_complex (z1, z2) =
   make_from_mag_ang(magnitude z1 * magnitude z2,
                     angle z1 + angle z2)
fun div_complex (z1, z2) =
   make_from_mag_ang(magnitude z1 / magnitude z2,
                     angle z1 - angle z2)

(* Constructors for complex numbers *)
fun make_from_real_imag (x, y) =
   make_from_real_imag_rectangular(x, y)
fun make_from_mag_ang (r, a) =
   make_from_mag_ang_polar(r, a)

(* 2.4.3 Multiple Representations for Abstract Data - Data-Directed Programming and Additivity *)

(* Note: The Scheme code is demonstrating both packaging (encapsulation) and
         dynamic dispatch.  ML can handle encapsulation with no problem using
         either functors or records.  I have been assured that ML can handle
         dynamic dispatch.  When I get time, I'll try to reformulate the translation.
         http://groups.google.com/group/comp.lang.functional/msg/aabbe98b97f8130d  *)

signature IMAGINARY =
   sig
      type imaginary
      val make_from_real_imag : real * real -> imaginary
      val make_from_mag_ang   : real * real -> imaginary
      val magnitude           : imaginary -> real
      val angle               : imaginary -> real
      val real_part           : imaginary -> real
      val imag_part           : imaginary -> real
   end

structure Rectangular :> IMAGINARY =
   struct
      type imaginary = real list
      fun make_from_real_imag (x, y) = [x, y]
      fun make_from_mag_ang (r, a) =
         [r * Math.cos a,  r * Math.sin a]
      fun real_part z = hd z
      fun imag_part z = hd(tl z)
      fun magnitude z =
         Math.sqrt(square_real(real_part z)) + square_real(imag_part z)
      fun angle z = Math.atan2(imag_part z, real_part z)
   end

structure Polar :> IMAGINARY =
   struct
      type imaginary = real list
      fun make_from_real_imag (x, y) =
         [Math.sqrt(square_real x + square_real y), Math.atan2(y, x)]
      fun make_from_mag_ang (r, a) = [r, a]
      fun magnitude z = hd z : real
      fun angle z = hd(tl z)
      fun real_part z = magnitude z * Math.cos(angle z)
      fun imag_part z = magnitude z * Math.sin(angle z)
   end

signature COMPLEX =
   sig
      include IMAGINARY
      val add_complex : imaginary * imaginary -> imaginary
      val sub_complex : imaginary * imaginary -> imaginary
      val mul_complex : imaginary * imaginary -> imaginary
      val div_complex : imaginary * imaginary -> imaginary
   end

functor Complex (Imag : IMAGINARY) :> COMPLEX =
   struct
      open Imag
      fun add_complex (z1, z2) =
         make_from_real_imag(real_part z1 + real_part z2,
                             imag_part z1 + imag_part z2)
      fun sub_complex (z1, z2) =
         make_from_real_imag(real_part z1 - real_part z2,
                             imag_part z1 - imag_part z2)
      fun mul_complex (z1, z2) =
         make_from_mag_ang(magnitude z1 * magnitude z2,
                           angle z1 + angle z2)
      fun div_complex (z1, z2) =
         make_from_mag_ang(magnitude z1 / magnitude z2,
                           angle z1 - angle z2)
   end

(* install *)
structure CR = Complex(Rectangular)
val _ = CR.make_from_real_imag(1.0, 2.0)
val _ = CR.make_from_mag_ang(1.0, 2.0)

structure CP = Complex(Polar)
val _ = CP.make_from_real_imag(1.0, 2.0)
val _ = CP.make_from_mag_ang(1.0, 2.0)

(* Alice ML version *)
   (* Note: Alice provides additional capabilities for importing modules. *)
   val complex = pack (Complex(Rectangular)) : COMPLEX;
   structure CR = unpack (complex) : COMPLEX
   val _ = CR.make_from_real_imag(1.0, 2.0)
   val _ = CR.make_from_mag_ang(1.0, 2.0)

   val complex = pack (Complex(Polar)) : COMPLEX;
   structure CP = unpack (complex) : COMPLEX
   val _ = CP.make_from_real_imag(1.0, 2.0)
   val _ = CP.make_from_mag_ang(1.0, 2.0)
(* end  Alice ML version *)

(* footnote *)
val _ = foldl op+ 0 [1, 2, 3, 4]

(* ML does not have corresponding apply or generic selectors *)

(* EXERCISE 2.73 *)
(* exercise left to reader to define appropriate functions
fun deriv (Number x, var) = Number 0
  | deriv (Variable x, Variable y) =
      if x = y
         then Number 1
         else Number 0
  | deriv (Variable x, var) = Number 0
  | deriv (Sum (x, y), var) =
      make_sum(deriv(x, var), deriv(y, var))
  | deriv (Product(x, y), var) =
      make_sum(make_product(x, deriv(y, var)), make_product(deriv(x, var), y))
*)
fun operator exp = exp
fun operands exp = exp
fun get_deriv x y z = Number 0

fun deriv (Number x, var) = Number 0
  | deriv (Variable x, Variable y) =
      if x = y
         then Number 1
         else Number 0
  | deriv (Variable x, var) = Number 0
  | deriv (exp, var) =
      get_deriv (operator exp) (operands exp) var

(* Message passing *)
fun make_from_real_imag (x, y) =
   {
      real_part = x,
      imag_part = y,
      magnitude = Math.sqrt(square_real x + square_real y),
      angle = Math.atan2(y, x)
   }
