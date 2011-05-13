// Functions defined in previous chapters
define function gcd_(a :: <integer>, b :: <integer>) => result  :: <integer>;
  if (b == 0)
    a
  else
    gcd_(b, modulo(a, b))
  end if;
end function gcd_;

define function fib(n :: <integer>) => result :: <integer>;
  case
    (n == 0)  => 0;
    (n == 1)  => 1;
    otherwise => fib(n - 1) + fib(n - 2);
  end case;
end function fib;

define function identity_(x :: <integer>) => result :: <integer>;
  x;
end function identity_;

// 2 Building Abstractions with Data
define function linear_combination(a, b, x, y)
  a * x + b * y;
end function linear_combination; 

define function mul(a, b)
  a * b;
end function mul; 

define function linear_combination_1(a, b, x, y)
  mul(a, x) + mul(b, y);
end function linear_combination_1; 

// 2.1.1 Introduction to Data Abstraction - Example: Arithmetic Operations for Rational Numbers
define function cons(x, y) => <pair>;
  pair(x, y);
end function cons;

define variable car = head;
define variable cdr = tail;

define function cadr(x)
  car(cdr(x));
end function cadr;

let x = cons(1, 2);
car(x);
cdr(x);

x := cons(1, 2);
let y = cons(3, 4);
let z = cons(x, y);

car(car(z));
car(cdr(z));

/* Literal Translation */
define function make_rat(n :: <integer>, d :: <integer>) => result :: <pair>;
  pair(n, d);
end function make_rat;

// Alternate: reducing to lowest terms in constructor
define function make_rat(n :: <integer>, d :: <integer>) => result :: <pair>;
  pair(floor/(n, gcd_(n, d)), floor/(d, gcd_(d, n)));
end function make_rat;

define function numer(x :: <pair>) => result :: <integer>;
  head(x);
end function numer;

define function denom(x :: <pair>) => result :: <integer>;
  tail(x);
end function denom;

define function add_rat(x :: <pair>, y :: <pair>) => result :: <pair>;
  make_rat(numer(x) * denom(y) + numer(y) * denom(x), denom(x) * denom (y));
end function add_rat;

define function sub_rat(x :: <pair>, y :: <pair>) => result :: <pair>;
  make_rat(numer(x) * denom(y) - numer(y) * denom(x), denom(x) * denom (y));
end function sub_rat;

define function mul_rat(x :: <pair>, y :: <pair>) => result :: <pair>;
  make_rat(numer(x) * numer(y), denom(x) * denom(y));
end function mul_rat;

define function div_rat(x :: <pair>, y :: <pair>) => result :: <pair>;
  make_rat(numer(x) * denom(y), denom(x) * numer(y));
end function div_rat;

define function equal_rat(x :: <pair>, y :: <pair>) => result :: <boolean>;
  (numer(x) * denom(y) == numer(y) * denom (x));
end function equal_rat;

define function print_rat(x :: <pair>) => ();
  format-out("%d/%d\n", numer(x), denom(x));
end function print_rat;

let one_half = make_rat(1, 2);
print_rat(one_half);

let one_third = make_rat(1, 3);
print_rat(add_rat(one_half, one_third));
print_rat(mul_rat(one_half, one_third));
print_rat(add_rat(one_third, one_third));

/* Exercise 2.1 */
define function make_rat_2(n :: <integer>, d :: <integer>) => result :: <pair>;
  if ((d < 0 & n < 0) | n < 0) d := d * -1; n := n * -1; end if;
  pair(n, d);
end function make_rat_2;

/* Object Translation */
define class <rat> (<object>)
  slot numer :: <integer>,
    required-init-keyword: numer:;
  slot denom :: <integer>,
    required-init-keyword: denom:;
end class <rat>;

define method initialize(this :: <rat>, #key lowest-terms :: <boolean> = #f)
  next-method();
  if (lowest-terms)
    let n = numer(this); let d = denom(this);
    numer-setter(floor/(n, gcd_(n, d)), this);
    denom-setter(floor/(d, gcd_(d, n)), this);
  end if;
end method initialize;

define method \+(x :: <rat>, y :: <rat>) => result :: <rat>;
  make(<rat>, numer: numer(x) * denom(y) + numer(y) * denom(x), denom: denom(x) * denom (y));
end method \+;

define method \-(x :: <rat>, y :: <rat>) => result :: <rat>;
  make(<rat>, numer: numer(x) * denom(y) - numer(y) * denom(x), denom: denom(x) * denom (y));
end method \-;

define method \*(x :: <rat>, y :: <rat>) => result :: <rat>;
  make(<rat>, numer: numer(x) * numer(y), denom: denom(x) * denom(y));
end method \*;

define method \/(x :: <rat>, y :: <rat>) => result :: <rat>;
  make(<rat>, numer: numer(x) * denom(y), denom: denom(x) * numer(y));
end method \/;

define method \=(x :: <rat>, y :: <rat>) => result :: <boolean>;
  (numer(x) * denom(y) = numer(y) * denom (x));
end method \=;

define method \<(x :: <rat>, y :: <rat>) => result :: <boolean>;
  (numer(x) * denom(y) < numer(y) * denom (x));
end method \<;

define method print(x :: <rat>) => ();
  format-out("%d/%d\n", numer(x), denom(x));
end method print;

let one_half = make(<rat>, numer: 1, denom: 2);
print(one_half);

let one_third = make(<rat>, numer: 1, denom: 3);
print(one_half + one_third);
print(one_half * one_third);
print(one_third + one_third);

// 2.1.2 Introduction to Data Abstraction - Abstraction barriers
/* Literal Translation */
define function make_rat(n :: <integer>, d :: <integer>) => result :: <pair>;
  pair(n, d);
end function make_rat;

define function numer(x :: <pair>) => result :: <integer>;
  let v = head(x); floor/(v, gcd_(v, tail(x)));
end function numer;

define function denom(x :: <pair>) => result :: <integer>;
  let v = tail(x); floor/(v, gcd_(v, head(x)));
end function denom;

/* Object Translation */
define class <rat> (<object>)
  slot numer_ :: <integer>,
    setter: #f,
    required-init-keyword: numer:;
  slot denom_ :: <integer>,
    setter: #f,
    required-init-keyword: denom:;
end class <rat>;

define method initialize(this :: <rat>, #key)
  next-method();
end method initialize;

define method numer(x :: <rat>) => result :: <integer>;
  let v = numer_(x); floor/(v, gcd_(v, denom_(x)));
end method numer;

define method denom(x :: <rat>) => result :: <integer>;
  let v = denom_(x); floor/(v, gcd_(v, numer_(x)));
end method denom;

/* Exercise 2.2 */
define function make_point(x :: <double-float>, y :: <double-float>) => result :: <pair>;
  pair(x, y);
end function make_point;

define function x_point(point :: <pair>) => result :: <double-float>;
  head(point);
end function x_point;

define function y_point(point :: <pair>) => result :: <double-float>;
  tail(point);
end function y_point;

define function make_segment(start_segment :: <pair>, end_segment :: <pair>) => result :: <pair>;
  pair(start_segment, end_segment);
end function make_segment;

define function start_segment(segment :: <pair>) => result :: <pair>;
  head(segment);
end function start_segment;

define function end_segment(segment :: <pair>) => result :: <pair>;
  tail(segment);
end function end_segment;

define function midpoint_segment(segment :: <pair>) => result :: <pair>;
  let s = start_segment(segment); let e = end_segment(segment);
  make_point((x_point(s) + x_point(e)) / 2.0d0, (y_point(s) + y_point(e)) / 2.0d0);
end function midpoint_segment;

define function print_point(point :: <pair>) => ();
  format-out("(%=,%=)\n", x_point(point), y_point(point));
end function print_point;

/* Exercise 2.3 */
define function length_segment(segment :: <pair>) => result :: <double-float>;
  let s = start_segment(segment); let e = end_segment(segment);
  sqrt(square_double(x_point(e) - x_point(s)) + square_double(y_point(e) - y_point(s)));
end function length_segment;

// Constructors create type tagged using <pair>
define function make_rectangle(anchor :: <pair>, xlen :: <double-float>, ylen :: <double-float>) => result :: <pair>;
  pair(#"AXY", pair(anchor, pair(xlen, ylen)));
end function make_rectangle;

define function make_rectangle_2(start_segment :: <pair>, end_segment :: <pair>) => result :: <pair>;
  pair(#"SEG", pair(start_segment, end_segment));
end function make_rectangle_2;

// 'length_rectangle' and 'width_rectangle' act as an abstraction barrier for higher-level
// procedures because 'rectangle' implementation details are buried here, and should the
// implementation change, only these procedures will need to be altered to support the change
define function length_rectangle(rectangle :: <pair>) => result :: <double-float>;
  // Determine rectangle representation type using type tag
  select (head(rectangle))
    #"AXY"    =>
      // Extract data for this rectangle representation type 
      let anchor = head(tail(rectangle));
      let xlen = head(tail(tail(rectangle)));
      let ylen = tail(tail(tail(rectangle)));
      // Compute length ...
      0.0d0;
    #"SEG"    =>
      // Extract data for this rectangle representation type 
      let start_segment = head(tail(rectangle));
      let end_segment = tail(tail(rectangle));
      // Compute length ...
      0.0d0;
    otherwise =>
      0.0d0;
  end select;
end function length_rectangle;

define function width_rectangle(rectangle :: <pair>) => result :: <double-float>;
  // As per 'length_rectangle' except that rectangle width is returned ...
  0.0d0;
end function width_rectangle;

// High-level procedures are quarantined from representation / implementation details
define function area_rectangle(rectangle :: <pair>) => result :: <double-float>;
  length_rectangle(rectangle) * width_rectangle(rectangle);
end function area_rectangle;

define function perimeter_rectangle(rectangle :: <pair>) => result :: <double-float>;
  length_rectangle(rectangle) * 2.0d0 + width_rectangle(rectangle) * 2.0d0;
end function perimeter_rectangle;

// 2.1.3 Introduction to Data Abstraction - What is meant by data?
define function cons(x, y) => result :: <function>;
  local
    method dispatch(m :: <integer>)
      case
        (m == 0)  => x;
        (m == 1)  => y;
        otherwise => concatenate("Argument not 0 or 1 - CONS: '", integer-to-string(m), "'");
      end case;
    end;

  dispatch;
end function cons;

define function car(z :: <function>)
  z(0);
end function car;

define function cdr(z :: <function>)
  z(1);
end function cdr;

/* Exercise 2.4 */
define function cons_1(x :: <object>, y :: <object>) => result :: <function>;
  method(m) m(x, y) end;
end function cons_1;

define function car_1(z :: <function>) => result :: <object>;
  z(method(x, y) x end);
end function car_1;

define function cdr_1(z :: <function>) => result :: <object>;
  z(method(x, y) y end);
end function cdr_1;

/* Exercise 2.5 */
define function cons_2(a :: <integer>, b :: <integer>) => result :: <integer>;
  2 ^ a * 3 ^ b;
end function cons_2;

define function car_2(i :: <integer>) => result :: <integer>;
  if (modulo(i, 2) == 0)
    car_2(floor/(i, 2)) + 1
  else
    0
  end if;
end function car_2;

define function cdr_2(i :: <integer>) => result :: <integer>;
  if (modulo(i, 3) == 0)
    cdr_2(floor/(i, 3)) + 1
  else
    0
  end if;
end function cdr_2;

/* Exercise 2.6 - unfinished */

// 2.1.4 Introduction to Data Abstraction - Extended Exercise: Interval Arithmetic
/* Literal Translation */
define function add_interval (x :: <pair>, y :: <pair>) => result :: <pair>;
  make_interval(lower_bound(x) + lower_bound(y), upper_bound(x) + upper_bound(y));
end function add_interval;

define function mul_interval (x :: <pair>, y :: <pair>) => result :: <pair>;
  let p1 = lower_bound(x) * lower_bound(y);
  let p2 = lower_bound(x) * upper_bound(y);
  let p3 = upper_bound(x) * lower_bound(y);
  let p4 = upper_bound(x) * upper_bound(y);
  make_interval(min(min(p1, p2), min(p3, p4)), max(max(p1, p2), max(p3, p4)));
end function mul_interval;

define function div_interval (x :: <pair>, y :: <pair>) => result :: <pair>;
  let z = make_interval(1.0d0 / upper_bound(y), 1.0d0 / lower_bound(y));
  mul_interval(x, z);
end function div_interval;

/* Exercise 2.7 */
define function make_interval(a :: <double-float>, b :: <double-float>) => result :: <pair>;
  pair(a, b);
end function make_interval;

define function lower_bound(i :: <pair>) => result :: <double-float>;
  head(i);
end function lower_bound;

define function upper_bound(i :: <pair>) => result :: <double-float>;
  tail(i);
end function upper_bound;

/* Exercise 2.8 */
define function sub_interval (x :: <pair>, y :: <pair>) => result :: <pair>;
  make_interval(lower_bound(x) - upper_bound(y), upper_bound(x) - lower_bound(y));
end function sub_interval;

/* Exercise 2.9 */
// Uses procedure 'width' from Ex. 2.11

let i = make_interval(5.0d0, 10.0d0);
let j = make_interval(15.0d0, 25.0d0);

// width of the sum (or difference) of two intervals *is* a function only of the widths of
// the intervals being added (or subtracted) 
format-out("%=\n", (width(add_interval(i, j)) == width(i) + width(j)));   // #t 
format-out("%=\n", (width(sub_interval(i, j)) == width(i) + width(j)));   // #t

// width of the product (or quotient) of two intervals *is not* a function only of the widths
// of the intervals being multiplied (or divided) 
format-out("%=\n", (width(mul_interval(i, j)) == width(i) + width(j)));   // #f
format-out("%=\n", (width(div_interval(i, j)) == width(i) + width(j)));   // #f

/* Exercise 2.10 */
define function zero_interval?(i :: <pair>) => result :: <boolean>;
  zero?(lower_bound(i)) | zero?(upper_bound(i));
end function zero_interval?;

define function div_interval_zero_check(x :: <pair>, y :: <pair>) => result :: <pair>;
  if (zero_interval?(y))
    error("Zero interval divisor"); 
  else
    div_interval(x, y)
  end if; 
end function div_interval_zero_check;

/* Exercise 2.11 - unfinished */
define function make_center_width(c :: <double-float>, w :: <double-float>) => result :: <pair>;
  make_interval(c - w, c + w);
end function make_center_width;

define function center(i :: <pair>) => result :: <double-float>;
  (lower_bound(i) + upper_bound(i)) / 2.0d0;
end function center;

define function width(i :: <pair>) => result :: <double-float>;
  (upper_bound(i) - lower_bound(i)) / 2.0d0;
end function width;

/* Exercise 2.12 */
define function make_center_percent(c :: <double-float>, p :: <double-float>) => result :: <pair>;
  make_center_width(c, p * c / 100.0d0);
end function make_center_percent;

define function percent(i :: <pair>) => result :: <double-float>;
  width(i) / center(i) * 100.0d0;
end function percent;

/* Object Translation */
define class <interval> (<object>)
  slot lower_bound :: <double-float>,
    setter: #f,
    required-init-keyword: lower_bound:;
  slot upper_bound :: <double-float>,
    setter: #f,
    required-init-keyword: upper_bound:;
end class <interval>;

define method \+(x :: <interval>, y :: <interval>) => result :: <interval>;
  make(<interval>, lower_bound: lower_bound(x) + lower_bound(y), upper_bound: upper_bound(x) + upper_bound(y));
end method \+;

define method \-(x :: <interval>, y :: <interval>) => result :: <interval>;
  make(<interval>, lower_bound: lower_bound(x) - upper_bound(y), upper_bound: upper_bound(x) - lower_bound(y));
end method \-;

define method \*(x :: <interval>, y :: <interval>) => result :: <interval>;
  let p1 = lower_bound(x) * lower_bound(y);
  let p2 = lower_bound(x) * upper_bound(y);
  let p3 = upper_bound(x) * lower_bound(y);
  let p4 = upper_bound(x) * upper_bound(y);
  make(<interval>, lower_bound: min(min(p1, p2), min(p3, p4)), upper_bound: max(max(p1, p2), max(p3, p4)));
end method \*;

define method \/(x :: <interval>, y :: <interval>) => result :: <interval>;
  let z = make(<interval>, lower_bound: 1.0d0 / upper_bound(y), upper_bound: 1.0d0 / lower_bound(y));
  x * z;
end method \/;

define method center(i :: <interval>) => result :: <double-float>;
  (lower_bound(i) + upper_bound(i)) / 2.0d0;
end method center;

define method width(i :: <interval>) => result :: <double-float>;
  (upper_bound(i) - lower_bound(i)) / 2.0d0;
end method width;

define method print(i :: <interval>) => ();
  format-out("%=, %=\n", lower_bound(i), upper_bound(i));
end method print;

/* Exercise 2.13 */
define function par1(r1 :: <pair>, r2 :: <pair>) => result :: <pair>;
  div_interval(mul_interval(r1, r2), add_interval(r1, r2));
end function par1;

define function par2(r1 :: <pair>, r2 :: <pair>) => result :: <pair>;
  let one = make_interval(1.0d0, 1.0d0);
  div_interval(one, add_interval(div_interval(one, r1), div_interval(one, r2)));
end function par2;

/* Exercise 2.14 - unfinished */

/* Exercise 2.15 - unfinished */

/* Exercise 2.16 - unfinished */

// 2.2.1 Hierarchical Data and the Closure Property - Representing Sequences
format-out("%=\n", pair(1, pair(2, pair(3, pair(4, #())))));

// May also do:
//
// format-out("%=\n", #(1, 2, 3, 4));
// format-out("%=\n", list(1, 2, 3, 4));
// format-out("%=\n", concatenate(#(1, 2), #(3, 4)));

let one-through-four = #(1, 2, 3, 4);

format-out("%=\n", one-through-four);
format-out("%=\n", head(one-through-four));
format-out("%=\n", tail(one-through-four));
format-out("%=\n", head(tail(one-through-four)));
format-out("%=\n", pair(10, one-through-four));

let squares = list(1, 4, 9, 16, 25);

define function list-ref(items :: <list>, n :: <integer>) => result :: <object>;
  if (n == 0)
    head(items)
  else
    list-ref(tail(items), n - 1)
  end if;
end function list-ref;

format-out("%=\n", list-ref(squares, 3));

define function length1(items :: <list>) => result :: <integer>;
  if (empty?(items))
    0
  else
    length1(tail(items)) + 1
  end if;
end function length1;

let odds = list(1, 3, 5, 7);

format-out("%=\n", length1(odds));

define function length2(items :: <list>) => result :: <integer>;
  local
    method length-iter(items :: <list>, count :: <integer>) => result :: <integer>;
      if (empty?(items))
        count
      else
        length-iter(tail(items), count + 1)
      end if;
    end;
  length-iter(items, 0);
end function length2;

define function append1(l1 :: <list>, l2 :: <list>) => result :: <list>;
  if (empty?(l1))
    l2
  else
    pair(head(l1), append1(tail(l1), l2))
  end if;
end function append1;

format-out("%=\n", append1(squares, odds));
format-out("%=\n", append1(odds, squares));

/* Exercise 2.17 */
define function last-pair1(items :: <list>) => result :: <list>;
  local
    method last-pair-iter(items :: <list>, prev :: <object>) => result :: <list>;
      if (empty?(items))
        list(prev)
      else
        last-pair-iter(tail(items), head(items))
      end if;
    end;
  if (empty?(items))
    #()
  else
    last-pair-iter(items, head(items));
  end if;
end function last-pair1;

/* Exercise 2.18 */
define function reverse1(items :: <list>) => result :: <list>;
  if (empty?(items))
    #()
  else
    append1(reverse1(tail(items)), list(head(items)))
  end if;
end function reverse1;

define function reverse2(items :: <list>) => result :: <list>;
  local
    method reverse-iter(items :: <list>, accum :: <list>) => result :: <list>;
      if (empty?(items))
        accum
      else
        reverse-iter(tail(items), pair(head(items), accum))
      end if;
    end;
  reverse-iter(items, #());
end function reverse2;

/* Exercise 2.19 */
define function no_more?(coin_values :: <list>) => result :: <boolean>;
  null?(coin_values);
end function no_more?;

define function except_first_denomination(coin_values :: <list>) => result :: <list>;
  tail(coin_values);
end function except_first_denomination;

define function first_denomination(coin_values :: <list>) => result :: type-union(<integer>, <float>);
  head(coin_values);
end function first_denomination;

define function cc(amount :: type-union(<integer>, <float>), coin_values :: <list>) => result :: type-union(<integer>, <float>);
  if (zero?(amount))
    1
  elseif (amount < 0 | no_more?(coin_values))
    0
  else
    cc(amount, except_first_denomination(coin_values)) +
    cc(amount - first_denomination(coin_values), coin_values)
  end if;
end function cc;

let us_coins = list(50, 25, 10, 5, 1);
let uk_coins = list(100, 50, 20, 10, 5, 2, 1, 0.5);

format-out("%=\n", cc(100, us_coins));  //    292
format-out("%=\n", cc(100, uk_coins));  // 104561

/* Exercise 2.20 */
define function filter1(pred :: <function>, items :: <list>) => result :: <list>;
  if (empty?(items))
    #()    
  elseif (pred(head(items)))
    pair(head(items), filter1(pred, tail(items)))
  else
    filter1(pred, tail(items))
  end if;
end function filter1;

define function same-parity1(arg1 :: <integer>, args :: <list>) => result :: <list>;
  let pred = if (odd?(arg1)) odd? else even? end if;
  filter1(pred, args);  
end function same-parity1;

define function same-parity2(arg1 :: <integer>, #rest args) => result :: <list>;
  let pred = if (odd?(arg1)) odd? else even? end if;
  filter1(pred, map-as(<list>, method(x) x end, args));  
end function same-parity2;

format-out("%=\n", same-parity1(1, list(2, 3, 4, 5)));
format-out("%=\n", same-parity2(1, 2, 3, 4, 5));

/* Mapping over lists */
define function scale-list(items :: <list>, factor :: <integer>) => result :: <list>;
  if (empty?(items))
    #()    
  else
    pair(head(items) * factor, scale-list(tail(items), factor))
  end if;
end function scale-list;

format-out("%=\n", scale-list(list(1, 2, 3, 4, 5), 10));

define function map1(proc :: <function>, items :: <list>) => result :: <list>;
  if (empty?(items))
    #()    
  else
    pair(proc(head(items)), map1(proc, tail(items)))
  end if;
end function map1;

format-out("%=\n", map1(method(x) x * x end, list(1, 2, 3, 4)));
format-out("%=\n", map1(abs, list(-10, 2.5, -11.6, 17)));

define function scale-list2(items :: <list>, factor :: <integer>) => result :: <list>;
  map1(method(x) x * factor end, items);
end function scale-list2;

format-out("%=\n", scale-list2(list(1, 2, 3, 4, 5), 10));

/* Exercise 2.21 */
define function square-list(items :: <list>) => result :: <list>;
  if (empty?(items))
    #()    
  else
    pair(head(items) * head(items), square-list(tail(items)))
  end if;
end function square-list;

define function square-list2(items :: <list>) => result :: <list>;
  map1(method(x) x * x end, items);
end function square-list2;

format-out("%=\n", square-list(list(1, 2, 3, 4, 5)));
format-out("%=\n", square-list2(list(1, 2, 3, 4, 5)));

/* Exercise 2.22 */
define function square-list3(items :: <list>) => result :: <list>;
  local
    method square-list-iter(items :: <list>, accum :: <list>) => result :: <list>;
      if (empty?(items))
        accum
      else
        square-list-iter(tail(items), pair(head(items) * head(items), accum))
      end if;
    end;
  square-list-iter(items, #());
end function square-list3;

define function square-list4(items :: <list>) => result :: <list>;
  local
    method square-list-iter(items :: <list>, accum :: <list>) => result :: <list>;
      if (empty?(items))
        accum
      else
        square-list-iter(tail(items), append1(accum, pair(head(items) * head(items), #())))
      end if;
    end;
  square-list-iter(items, #());
end function square-list4;

define function square-list5(items :: <list>) => result :: <list>;
  local
    method square-list-iter(items :: <list>, accum :: <list>) => result :: <list>;
      if (empty?(items))
        accum
      else
        square-list-iter(tail(items), pair(head(items) * head(items), accum))
      end if;
    end;
  reverse(square-list-iter(items, #()));
end function square-list5;

// Reverse order, but fast because a single list traversal occurs to build up list
format-out("%=\n", square-list3(list(1, 2, 3, 4, 5)));

// Correct order; slow due to append operation performed on each list element
format-out("%=\n", square-list4(list(1, 2, 3, 4, 5)));

// Correct order; fast because list is built up in a single traversal, and another
// single traversal sees it reversed
format-out("%=\n", square-list5(list(1, 2, 3, 4, 5)));

/* Exercise 2.23 */
define function for-each1(proc :: <function>, items :: <list>) => ();
  if (~(empty?(items)))
    proc(head(items));
    for-each1(proc, tail(items))
  end if;
end function for-each1;

for-each1(method(x) format-out("%=\n", x) end, list(-10, 2.5, -11.6, 17));

// 2.2.2 Hierarchical Data and the Closure Property - Hierarchical Structures
define function count-leaves(items :: <object>) => result :: <integer>;
  if (null?(items))
    0
  elseif (pair?(items))
    count-leaves(head(items)) + count-leaves(tail(items)) 
  else
    1
  end if;
end function count-leaves;

/* Exercise 2.24 */
list(1, list(2, list(3, 4)));  // #(1, #(2, #(3, 4)))

/* Exercise 2.25 */
let l1 = list(1, 3, list(5, 7), 9);
let l2 = list(list(7));
let l3 = list(1, list(2, list(3, list(4, list(5, list(6, 7))))));

format-out("%=\n", head(tail(head(tail(tail(l1))))));
format-out("%=\n", head(head(l2)));
format-out("%=\n", head(tail(head(tail(head(tail(head(tail(head(tail(head(tail(l3)))))))))))));

/* Exercise 2.26 */
let x = list(1, 2, 3);
let y = list(4, 5, 6);

append1(x, y);                // #(1, 2, 3, 4, 5, 6)
pair(x, y);                   // #(#(1, 2, 3), 4, 5, 6)
list(x, y);                   // #(#(1, 2, 3), #(4, 5, 6))

/* Exercise 2.27 */
define function deep-reverse(items :: <list>) => result :: <list>;
  if (empty?(items))
    #()
  elseif (pair?(head(items)))
    append1(deep-reverse(tail(items)), list(deep-reverse(head(items))))
  else
    append1(deep-reverse(tail(items)), list(head(items)))
  end if;
end function deep-reverse;

let x = list(list(1, 2), list(3, 4));

deep-reverse(x);              // #(#(4, 3), #(2, 1))
reverse1(x);                  // #(#(3, 4), #(1, 2)) 

/* Exercise 2.28 */
define function fringe(items :: <list>) => result :: <list>;
  if (empty?(items))
    #()
  elseif (pair?(head(items)))
    append1(fringe(head(items)), fringe(tail(items)))
  else
    pair(head(items), fringe(tail(items)))
  end if;
end function fringe;

let x = list(list(1, 2), list(3, 4));

fringe(x);                    // #(1, 2, 3, 4)
fringe(list(x, x));           // #(1, 2, 3, 4, 1, 2, 3, 4)

/* Exercise 2.29 */
/* List-based representation */

// a.
define function make-mobile(left :: <list>, right :: <list>) => result :: <list>;
  list(left, right);
end function make-mobile;

define function make-branch(length :: <integer>, structure :: type-union(<integer>, <list>)) => result :: <list>;
  list(length, structure);
end function make-branch;

define function left-branch(mobile :: <list>) => result :: <list>;
  head(mobile);
end function left-branch;

define function right-branch(mobile :: <list>) => result :: <list>;
  head(tail(mobile));
end function right-branch;

define function branch-length(branch :: <list>) => result :: <integer>;
  head(branch);
end function branch-length;

define function branch-structure(branch :: <list>) => result :: type-union(<integer>, <list>);
  head(tail(branch));
end function branch-structure;

// Helpers for b. and c.
define function branch-weight(branch :: <object>) => result :: <integer>;
  if (null?(branch))
    0
  else
    if (pair?(branch-structure(branch)))
      branch-weight(branch-structure(branch))
    else
      branch-structure(branch)
    end if;
  end if;
end function branch-weight;

define function total-branch-length(branch :: <object>) => result :: <integer>;
  if (null?(branch))
    0
  else
    if (pair?(branch-structure(branch)))
      branch-length(branch) + total-branch-length(branch-structure(branch))
    else
      branch-length(branch)
    end if;
  end if;
end function total-branch-length;

// b.
define function total-weight(mobile :: <list>) => result :: <integer>;
  branch-weight(left-branch(mobile)) + branch-weight(right-branch(mobile));
end function total-weight;

// c. [Not as per specification]
define function mobile-balanced?(mobile :: <list>) => result :: <boolean>;
  let lmwl = total-branch-length(left-branch(mobile)) * branch-weight(left-branch(mobile));
  let rmwl = total-branch-length(right-branch(mobile)) * branch-weight(right-branch(mobile));
  lmwl == rmwl;
end function mobile-balanced?;

// d.
/* Pair-based representation - unfinished */

/* Mapping over trees */
define function scale-tree(tree :: <object>, factor :: <integer>) => result :: <object>;
  if (null?(tree))
    #()    
  elseif (pair?(tree))
    pair(scale-tree(head(tree), factor), scale-tree(tail(tree), factor))
  else
    tree * factor
  end if;
end function scale-tree;

define function scale-tree2(tree :: <object>, factor :: <integer>) => result :: <object>;
  map1(method(sub-tree)
         if (pair?(sub-tree)) scale-tree(sub-tree, factor) else sub-tree * factor end if;
       end,
     tree);
end function scale-tree2;

let tree = list(1, list(2, list(3, 4), 5), list(6, 7));

format-out("%=\n", scale-tree(tree, 10));
format-out("%=\n", scale-tree2(tree, 10));

/* Exercise 2.30 */
define function square-tree(tree :: <list>) => result :: <list>;
  if (empty?(tree))
    #()    
  elseif (pair?(head(tree)))
    append1(list(square-tree(head(tree))), square-tree(tail(tree)))
  else
    pair(head(tree) * head(tree), square-tree(tail(tree)))
  end if;
end function square-tree;

let x = list(1, list(2, list(3, 4), 5), list(6, 7));

square-tree(x);               // #(1, #(4, #(9, 16), 25), #(36, 49))

/* Exercise 2.31 */
define function treemap1(proc :: <function>, tree :: <list>) => result :: <list>;
  if (empty?(tree))
    #()    
  elseif (pair?(head(tree)))
    append1(list(treemap1(proc, head(tree))), treemap1(proc, tail(tree)))
  else
    pair(proc(head(tree)), treemap1(proc, tail(tree)))
  end if;
end function treemap1;

define function square-tree2(tree :: <list>) => result :: <list>;
  treemap1(method(x) x * x end, tree);
end function square-tree2;

let x = list(1, list(2, list(3, 4), 5), list(6, 7));

square-tree2(x);              // #(1, #(4, #(9, 16), 25), #(36, 49))

/* Exercise 2.32 */
define function subsets(s :: <list>) => result :: <list>;
  if (empty?(s))
    list(#())
  else
    let rest = subsets(tail(s));
    append1(rest, map1(method(x) pair(head(s), x) end, rest))
  end if;
end function subsets;

let s = list(1, 2, 3);

subsets(s);                   // #(#(), #(3), #(2), #(2, 3), #(1), #(1, 3), #(1, 2), #(1, 2, 3))
