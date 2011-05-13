(* SICP Chapter #02 Examples in F# *)
#light
(* Functions defined in previous chapters *)
let rec gcd a b =
   match b with
    | 0 -> a
    | b -> gcd b (a % b)

let rec fib n =
   match n with
    | 0 -> 0
    | 1 -> 1
    | n -> fib(n - 1) + fib(n - 2)

let identity x = x

(* Utility functions *)
(* Convert a list of characters to a string *)
let rec implode = function
   | [] -> ""
   | charlist -> String.of_char(List.hd charlist) ^ implode (List.tl charlist)

(* 2 Building Abstractions with Data *)
let linear_combination a b x y = a*x + b*y

let mulx = ( * )
let linear_combination' a b x y =
   mulx a x + mulx b y

(* 2.1.1 Introduction to Data Abstraction - Example: Arithmetic Operations for Rational Numbers *)

(* Literal Translation *)
let make_rat n d = [n; d]
let numer x = List.hd x
let denom x = List.hd(List.tl x)

let add_rat x y =
   make_rat ((numer x * denom y) + (numer y * denom x)) (denom x * denom y)
let sub_rat x y =
   make_rat ((numer x * denom y) - (numer y * denom x)) (denom x * denom y)
let mul_rat x y =
   make_rat (numer x * numer y) (denom x * denom y)
let div_rat x y =
   make_rat (numer x * denom y) (denom x * numer y)
let equal_rat x y =
   ((numer x * denom y) = (numer y * denom x))

let cons x y = [x; y]
let car = List.hd
let cdr = List.tl
let compose f g x = f(g x)
let cadr x = car(cdr x)

let x = cons 1 2
car x
cdr x

let x' = cons 1 2
let y = cons 3 4
let z = cons x' y
car(car z)
car(cdr z)

(* footnote -- alternative definitions *)
let make_rat'' = cons
let numer'' = car
let denom'' = cadr

let print_rat x =
   print_string ("\n" ^ (string_of_int (numer x)) ^ "/" ^ (string_of_int (denom x)))

let one_half = make_rat 1 2
print_rat one_half

let one_third = make_rat 1 3
print_rat (add_rat one_half one_third)
print_rat (mul_rat one_half one_third)
print_rat (add_rat one_third one_third)

(* reducing to lowest terms in constructor *)
let make_rat' n d =
   let g = gcd n d
   in [n / g; d / g]

let add_rat' x y =
   make_rat' ((numer x * denom y) + (numer y * denom x)) (denom x * denom y)

print_rat(add_rat' one_third one_third)
(* end Literal Translation *)

module sicp_module_translation =
(* Module Translation *)
   module Rational =
      struct
         type rational = int list
         let make_rat n d = [n; d]
         let numer x = List.hd x
         let denom x = List.hd(List.tl x)
         let add_rat x y =
            make_rat ((numer x * denom y) + (numer y * denom x)) (denom x * denom y)
         let sub_rat x y =
            make_rat ((numer x * denom y) - (numer y * denom x)) (denom x * denom y)
         let mul_rat x y =
            make_rat (numer x * numer y) (denom x * denom y)
         let div_rat x y =
            make_rat (numer x * denom y) (denom x * numer y)
         let equal_rat x y =
            ((numer x * denom y) = (numer y * denom x))
         let print_rat x =
            print_string ("\n" ^ string_of_int (numer x) ^ "/" ^ string_of_int  (denom x))
      end

   let one_half = Rational.make_rat 1 2
   Rational.print_rat one_half

   let one_third = Rational.make_rat 1 3
   Rational.print_rat (Rational.add_rat one_half one_third)
   Rational.print_rat (Rational.mul_rat one_half one_third)
   Rational.print_rat (Rational.add_rat one_third one_third)

   (* reducing to lowest terms in constructor *)
   module Rational' =
      struct
         type rational = int list
         let make_rat n d =
            let g = gcd n d
            in [n / g; d / g]
         let numer x = List.hd x
         let denom x = List.hd(List.tl x)
         let add_rat x y =
            make_rat ((numer x * denom y) + (numer y * denom x)) (denom x * denom y)
         let sub_rat x y =
            make_rat ((numer x * denom y) - (numer y * denom x)) (denom x * denom y)
         let mul_rat x y =
            make_rat (numer x * numer y) (denom x * denom y)
         let div_rat x y =
            make_rat (numer x * denom y) (denom x * numer y)
         let equal_rat x y =
            ((numer x * denom y) = (numer y * denom x))
         let print_rat x =
            print_string ("\n" ^ string_of_int (numer x) ^ "/" ^ string_of_int (denom x))
      end

   let one_third' = Rational'.make_rat 1 3
   Rational'.print_rat (Rational'.add_rat one_third' one_third')
(* end Module Translation *)

module sicp_object_translation =
(* Object Translation *)
   type Rational(n, d) =
      class
         member x.numer = n
         member x.denom = d
         member x.add_rat (other : Rational) =
            new Rational(x.numer * other.denom + other.numer * x.denom, x.denom * other.denom)
         member x.sub_rat (other : Rational) =
            new Rational(x.numer * other.denom - other.numer * x.denom, x.denom * other.denom)
         member x.mul_rat (other : Rational) =
            new Rational(x.numer * other.numer, x.denom * other.denom)
         member x.div_rat (other : Rational) =
            new Rational(x.numer * other.denom, x.denom * other.numer)
         member x.equal_rat (other : Rational) =
            ((x.numer * other.denom) = (other.numer * x.denom))
         member x.print_rat () =
            print_string ("\n" ^ string_of_int (x.numer) ^ "/" ^ string_of_int (x.denom))
      end

   let one_half = new Rational(1, 2)
   one_half.print_rat()

   let one_third = new Rational(1, 3)
   (one_half.add_rat one_third).print_rat()
   (one_half.mul_rat one_third).print_rat()
   (one_third.add_rat one_third).print_rat()

   (* reducing to lowest terms in constructor *)
   type Rational' (n, d) =
      class
         let g = gcd n d
         member x.numer = n / g
         member x.denom = d / g
         member x.add_rat (other : Rational') =
            new Rational(x.numer * other.denom + other.numer * x.denom, x.denom * other.denom)
         member x.sub_rat (other : Rational') =
            new Rational(x.numer * other.denom - other.numer * x.denom, x.denom * other.denom)
         member x.mul_rat (other : Rational') =
            new Rational(x.numer * other.numer, x.denom * other.denom)
         member x.div_rat (other : Rational') =
            new Rational(x.numer * other.denom, x.denom * other.numer)
         member x.equal_rat (other : Rational') =
            ((x.numer * other.denom) = (other.numer * x.denom))
         member x.print_rat () =
            print_string ("\n" ^ string_of_int (x.numer) ^ "/" ^ string_of_int (x.denom))
      end

   let one_third' = new Rational'(1, 3)
   (one_third'.add_rat one_third').print_rat()
(* end Object Translation *)

(* Exercise 2.1 *)
let make_rat''' n d =
   if (d < 0 && n < 0) || n < 0
      then [-d; -n]
      else [d; n]

(* 2.1.2 Introduction to Data Abstraction - Abstraction barriers *)

module sicp_literal_translation' =
(* Literal Translation *)
   (* reducing to lowest terms in selectors *)
   let make_rat n d = cons n d

   let numer x =
      let g = gcd (car x) (cadr x)
      in car x / g
   let denom x =
      let g = gcd (car x) (cadr x)
      in cadr x / g
(* end Literal Translation *)

module sicp_module_translation' =
(* Module Translation *)
   (* reducing to lowest terms in selectors *)
   module Rational =
      struct
         type rational = int list
         let make_rat n d = [n; d]
         let numer x =
            let n = List.hd x
            and d = List.hd (List.tl x)
            in n / (gcd n d)
         let denom x =
            let n = List.hd x
            and d = List.hd(List.tl x)
            in d / (gcd n d)
         let add_rat x y =
            make_rat ((numer x * denom y) + (numer y * denom x)) (denom x * denom y)
         let sub_rat x y =
            make_rat ((numer x * denom y) - (numer y * denom x)) (denom x * denom y)
         let mul_rat x y =
            make_rat (numer x * numer y) (denom x * denom y)
         let div_rat x y =
            make_rat (numer x * denom y) (denom x * numer y)
         let equal_rat x y =
            ((numer x * denom y) = (numer y * denom x))
         let print_rat x =
            print_string ("\n" ^ string_of_int (numer x) ^ "/" ^ string_of_int  (denom x))
      end
(* end Module Translation *)

module sicp_object_translation' =
(* Object Translation *)
   (* reducing to lowest terms in selectors *)
   type Rational(n, d) =
      class
         member x.numer = n / gcd n d
         member x.denom = d / gcd n d
         member x.add_rat (other : Rational) =
            new Rational(x.numer * other.denom + other.numer * x.denom, x.denom * other.denom)
         member x.sub_rat (other : Rational) =
            new Rational(x.numer * other.denom - other.numer * x.denom, x.denom * other.denom)
         member x.mul_rat (other : Rational) =
            new Rational(x.numer * other.numer, x.denom * other.denom)
         member x.div_rat (other : Rational) =
            new Rational(x.numer * other.denom, x.denom * other.numer)
         member x.equal_rat (other : Rational) =
            ((x.numer * other.denom) = (other.numer * x.denom))
         member x.print_rat () =
            print_string ("\n" ^ string_of_int (x.numer) ^ "/" ^ string_of_int (x.denom))
      end
(* end Object Translation *)

(* Exercise 2.2 *)
let make_point x y = [x; y]
let x_point point = List.hd point
let y_point point = List.hd(List.tl point)
let make_segment start_segment end_segment = [start_segment; end_segment]
let start_segment' segment = List.hd segment
let end_segment' segment = List.hd(List.tl segment)
let midpoint_segment segment =
   let s = start_segment' segment
   and e = end_segment' segment
   in make_point ((x_point s + x_point e) / 2.0) ((y_point s + y_point e) / 2.0)
let print_point p =
   print_string ("\n" ^ "(" ^ string_of_float (x_point p) ^ "," ^ string_of_float (y_point p) ^ ")")

(* Exercise 2.3 *)
let square_float x = x * x
let length_segment segment =
   let s = start_segment' segment
   and e = end_segment' segment
   in sqrt(square_float(x_point e - x_point s) + square_float(y_point e - y_point s))

(* Constructors create type tagged using <pair> *)
type 'a axy = { anchor:'a; xlen:'a; ylen:'a }
type 'a seg = { start_segment:'a; end_segment:'a }
type 'a rectangle = Axy of 'a axy
                  | Seg of 'a seg
let make_rectangle anchor xlen ylen =
   Axy {anchor=anchor; xlen=xlen; ylen=ylen}
let make_rectangle' start_segment end_segment =
   Seg {start_segment=start_segment; end_segment=end_segment}

(* 'length_rectangle' and 'width_rectangle' act as an abstraction barrier for higher-level
   procedures because 'rectangle' implementation details are buried here, and should the
   implementation change, only these procedures will need to be altered to support the change *)
let length_rectangle rect =
   match rect with
    | Axy {anchor=anchor; xlen=xlen; ylen=ylen} -> 0.0                     (* Compute length ... *)
    | Seg {start_segment=start_segment; end_segment=end_segment} -> 0.0    (* Compute length ... *)

let width_rectangle rect =
   (* As per 'length_rectangle' except that rectangle width is returned ... *)
   0.0

(* High-level procedures are quarantined from representation / implementation details *)
let area_rectangle rect =
   (length_rectangle rect) * (width_rectangle rect)

let perimeter_rectangle rect =
   (length_rectangle rect) * 2.0 + (width_rectangle rect) * 2.0

(* 2.1.3 Introduction to Data Abstraction - What is meant by data? *)

exception IllFormedExpr of string
let cons' x y m =
   match m with
    | 0 -> x
    | 1 -> y
    | _ -> raise (IllFormedExpr ("Argument not 0 or 1 -- CONS " ^ string_of_int m))

let car' z = z 0
let cdr' z = z 1

(* Exercise 2.4 *)
let cons'' x y m = m x y
let car'' z = z (fun p q -> p)
let cdr'' z = z (fun p q -> q)

(* Exercise 2.5 *)
let rec power x k =
   match k with
    | 0 -> 1
    | 1 -> x
    | _ -> x * power x (k-1)
let cons''' x y =
   power 2 (x * power 3 y)
let rec car''' z =
   if (z % 2) = 0
      then car''' ((z / 2) + 1)
      else 0
let rec cdr''' z =
   if (z % 3) = 0
      then cdr''' ((z / 3) + 1)
      else 0

(* Exercise 2.6 *)
let zero f x = x
let add1 n f x = f (n f x)

(* 2.1.4 Introduction to Data Abstraction - Extended Exercise: Interval Arithmetic *)

(* Literal Translation *)
let make_interval a b = (a, b)

let lower_bound (x, y) = x
let upper_bound (x, y) = y

let add_interval x y =
   make_interval (lower_bound x + lower_bound y) (upper_bound x + upper_bound y)

let mul_interval x y =
   let p1 = lower_bound x * lower_bound y
   and p2 = lower_bound x * upper_bound y
   and p3 = upper_bound x * lower_bound y
   and p4 = upper_bound x * upper_bound y
   in make_interval
      (min (min p1 p2) (min p3 p4))
      (max (max p1 p2) (max p3 p4))

let div_interval x y =
   let z = make_interval (1.0 / upper_bound y) (1.0 / lower_bound y)
   in mul_interval x z

let make_center_width c w =
   make_interval (c - w) (c + w)

let center i =
   (lower_bound i + upper_bound i) / 2.0

let width i =
   (upper_bound i - lower_bound i) / 2.0

(* parallel resistors *)
let par1 r1 r2 =
   div_interval (mul_interval r1 r2) (add_interval r1 r2)

let par2 r1 r2 =
   let one = make_interval 1.0 1.0
   in div_interval
      one
      (add_interval (div_interval one r1) (div_interval one r2))
(* end Literal Translation *)

module sicp_module_translation'' =
(* Module Translation *)
   module Interval =
      struct
         type interval = float * float
         let make_interval a b = (a, b)
         let lower_bound (x, y) = x
         let upper_bound (x, y) = y
         let add_interval x y =
            make_interval (lower_bound x + lower_bound y) (upper_bound x + upper_bound y)
         let mul_interval x y =
            let p1 = lower_bound x * lower_bound y
            and p2 = lower_bound x * upper_bound y
            and p3 = upper_bound x * lower_bound y
            and p4 = upper_bound x * upper_bound y
            in make_interval
               (min (min p1 p2) (min p3 p4))
               (max (max p1 p2) (max p3 p4))
         let div_interval x y =
            let z = make_interval (1.0 / upper_bound y) (1.0 / lower_bound y)
            in mul_interval x z
         let make_center_width c w =
            make_interval (c - w) (c + w)
         let center i =
            (lower_bound i + upper_bound i) / 2.0
         let width i =
            (upper_bound i - lower_bound i) / 2.0
      end

   (* parallel resistors *)
   let par1 r1 r2 =
      Interval.div_interval (Interval.mul_interval r1 r2) (Interval.add_interval r1 r2)

   let par2 r1 r2 =
      let one = Interval.make_interval 1.0 1.0
      in Interval.div_interval
         one
         (Interval.add_interval (Interval.div_interval one r1)
                                (Interval.div_interval one r2))
(* end Module Translation *)

module sicp_object_translation'' =
(* Object Translation *)
   type Interval(a, b) =
      class
         member x.lower_bound = a
         member y.upper_bound = b
         member x.add_interval (other : Interval) =
            new Interval(x.lower_bound + other.lower_bound, x.upper_bound + other.upper_bound)
         member x.mul_interval (other : Interval) =
            let p1 = x.lower_bound * other.lower_bound
            and p2 = x.lower_bound * other.upper_bound
            and p3 = x.upper_bound * other.lower_bound
            and p4 = x.upper_bound * other.upper_bound
            in new Interval(min (min p1 p2) (min p3 p4), max (max p1 p2) (max p3 p4))
         member x.div_interval (other : Interval) =
            let z = new Interval(1.0 / other.upper_bound, 1.0 / other.lower_bound)
            in x.mul_interval z
         member x.make_center_width c w =
            new Interval(c - w, c + w)
         member x.center =
            (x.lower_bound + x.upper_bound) / 2.0
         member x.width =
            (x.upper_bound - x.lower_bound) / 2.0
      end

   (* parallel resistors *)
   let par1 (r1 : Interval) (r2 : Interval) =
      (r1.mul_interval r2).div_interval(r1.add_interval r2)

   let par2 r1 r2 =
      let one = new Interval(1.0, 1.0)
      in one.div_interval((one.div_interval r1).add_interval(one.div_interval r2))
(* end Object Translation *)

(* Exercise 2.7 *)
let make_interval' a b = (a, b)
let lower_bound' (x, y) = x
let upper_bound' (x, y) = y

(* Exercise 2.8 *)
let sub_interval x y =
   make_interval (lower_bound x - lower_bound y) (upper_bound x - upper_bound y)

(* Exercise 2.9 *)
let i = make_interval 5.0 10.0
let j = make_interval 15.0 25.0

(* width of the sum (or difference) of two intervals *is* a function only of the widths of
   the intervals being added (or subtracted) *)
let _ = (width (add_interval i j), (width i + width j))
let _ = (width (sub_interval i j), (width i + width j))

(* width of the product (or quotient) of two intervals *is not* a function only of the widths
   of the intervals being multiplied (or divided) *)
let _ = (width (mul_interval i j), (width i + width j))
let _ = (width (div_interval i j), (width i + width j))

(* Exercise 2.10 *)
exception DivideByZero of string
let is_zero_interval i =
   lower_bound i = 0.0 || upper_bound i = 0.0
let div_interval_zero_check x y =
   if is_zero_interval y
      then raise (DivideByZero("Zero interval divisor"))
      else div_interval x y

(* Exercise 2.12 *)
let make_center_percent c p =
   make_center_width c (p * c / 100.0)
let percent i =
   width i / (center i) * 100.0

(* 2.2.1 Hierarchical Data and the Closure Property - Representing Sequences *)

let _ = 1::2::3::4::[]
let one_through_four = [1; 2; 3; 4]

one_through_four
List.hd one_through_four
List.tl one_through_four
List.hd (List.tl one_through_four)
10::one_through_four

(* List.nth *)
let rec list_ref items n =
   match items, n with
    | x::xs, 0 -> x
    | x::xs, n -> list_ref xs (n-1)
    | [], _ -> failwith "Empty List"

let squares = [1; 4; 9; 16; 25]
list_ref squares 3

let length items =
   let rec length_iter items count =
      match items with
       | [] -> count
       | x::xs -> length_iter xs (1+count)
   in length_iter items 0

(* Alternate definition *)
let rec length' =
   function
    | [] -> 0
    | x::xs -> 1 + length' xs

let odds = [1; 3; 5; 7]
length' odds

(* From OCaml stdlib: *)
let rec length_aux len =
   function
    | [] -> len
    | a::l -> length_aux (len + 1) l

let length'' l = length_aux 0 l

let rec append list1 list2 =
   match list1 with
    | [] -> list2
    | x::xs -> x :: append xs list2

append squares odds
append odds squares

(* Mapping over lists *)
let rec scale_list factor items =
   match items with
    | [] -> []
    | x::xs -> x * factor :: scale_list factor xs

scale_list 10 [1; 2; 3; 4; 5]

let rec map f items =
   match items with
    | [] -> []
    | x::xs -> f x :: map f xs

(* Alternate definition *)
let rec map' f = function
  | [] -> []
  | x::xs -> f x :: map' f xs

map' abs_float [-10.0; 2.5; -11.6; 17.0]

map (fun x -> x * x) [1; 2; 3; 4]

let scale_list' factor items =
   map (fun x -> x * factor) items

List.map2 ( + ) [40;50;60] [700;800;900]
List.map2 (fun x y -> x+2*y) [1;2;3] [4;5;6]

(* Exercise 2.17 *)
let rec last_pair = function
  | [h1;h2] -> h1, h2
  | x::xs -> last_pair xs
  | [] -> invalid_arg "last_pair"
last_pair [23; 72; 149; 34]

(* Exercise 2.18 *)
let rec reverse items =
   match items with
    | [] -> []
    | x::xs -> append (reverse xs) [x]
reverse [1; 4; 9; 16; 25]
let reverse' items =
   let rec reverse_iter items accum =
      match items with
       | [] -> accum
       | x::xs -> reverse_iter xs (x::accum)
   in reverse_iter items []
reverse' [1; 4; 9; 16; 25]

(* Exercise 2.19 *)
let no_more coin_values =
   match coin_values with
    | [] -> true
    | x::xs -> false
let except_first_denomination coin_values = List.tl coin_values
let first_denomination coin_values = List.hd coin_values
let rec cc amount coin_values =
   if amount = 0
      then 1
      else
         if amount < 0 || no_more coin_values
            then 0
            else
               (cc amount (except_first_denomination coin_values)) +
               (cc (amount - first_denomination coin_values) coin_values)
let us_coins = [50; 25; 10; 5; 1]
cc 100 us_coins
(* Note: F# doesn't like mixing ints and floats (answer should be 104561) *)
(* let uk_coins = [100; 50; 20; 10; 5; 2; 1; 0.5] *)
(* cc 100 uk_coins *)

(* Exercise 2.20 *)
let rec filter predicate items =
   match items with
    | [] -> []
    | x::xs ->
      if predicate x
         then x :: filter predicate xs
         else filter predicate xs
let isOdd n = (n % 2 = 1)
let isEven = compose not isOdd
let same_parity xs =
   let predicate = if isOdd (List.hd xs) then isOdd else isEven
   in filter predicate (List.tl xs)
same_parity [1; 2; 3; 4; 5; 6; 7]
same_parity [2; 3; 4; 5; 6; 7]

(* Exercise 2.21 *)
let rec square_list items =
   match items with
    | [] -> []
    | x::xs ->  x*x :: square_list xs
square_list [1; 2; 3; 4]
let square_list_1 items =
   map (fun x -> x*x) items
square_list_1 [1; 2; 3; 4]

(* Exercise 2.22 *)
let square x = x * x
let square_list_2 list = map square list
let square_list_3 items =
   let rec iter things answer =
      match things with
       | [] -> answer
       | x::xs -> iter xs (square x::answer)
   in iter items []
square_list_3 [1; 2; 3; 4]
let square_list_4 items =
   let rec iter things answer =
      match things with
       | [] -> answer
       | x::xs -> iter xs (answer @ [square x])
   in iter items []
square_list_4 [1; 2; 3; 4]
let square_list_5 items =
   let rec iter things answer =
      match things with
       | [] -> answer
       | x::xs -> iter xs (square x::answer)
   in reverse (iter items [])
square_list_5 [1; 2; 3; 4]

(* Exercise 2.23 *)
(* See also List.iter *)
let rec for_each items f =
   match items with
    | [] -> ()
    | x::xs -> let _ = f x in for_each xs f
for_each [57; 321; 88] (fun x -> print_string ("\n" ^ (string_of_int x)));

(* 2.2.2 Hierarchical Data and the Closure Property - Hierarchical Structures *)

type 'a nestedlist = Leaf of 'a
                   | Node of 'a nestedlist list

let rec length''' = function
  | Node list -> length list
  | Leaf _ -> 1

Node[Node[Leaf 1; Leaf 2]; Node[Leaf 3; Leaf 4]]
let xa = Node[Node[Leaf 1; Leaf 2]; Node[Leaf 3; Leaf 4]]
length''' xa

let rec count_leaves = function
  | Node [] -> 0
  | Node(x::xs) -> count_leaves x + count_leaves(Node xs)
  | Leaf x -> 1

let xb = Node[Node[Leaf 1; Leaf 2]; Node[Leaf 3; Leaf 4]]
length''' xb
count_leaves xb

Node[xb; xb]
length''' (Node[xb; xb])
count_leaves (Node[xb; xb])

(* Mapping over trees *)
let rec scale_tree factor tree =
   match tree with
    | Leaf x -> Leaf (x * factor)
    | Node [] -> Node []
    | Node (x::xs) ->
         let a = scale_tree factor (Node xs) in
         let b =
            match a with
             | Node c -> c
             | Leaf c -> [Leaf c]
         in Node ((scale_tree factor x) :: b)

scale_tree 10 (Node[Leaf 1; Node[Leaf 2; Node[Leaf 3; Leaf 4]; Leaf 5]; Node[Leaf 6; Leaf 7]])

let rec scale_tree' factor tree =
   match tree with
    | Leaf x -> Leaf (x * factor)
    | Node x -> Node(map (scale_tree' factor) x)

(* Exercise 2.24 *)
Node[Leaf 1; Node[Leaf 2; Node[Leaf 3; Leaf 4]]]

(* Exercise 2.25 *)
Node[Leaf 1; Leaf 3; Node[Leaf 5; Leaf 7]; Leaf 9]
Node[Node[Leaf 7]]
Node[Leaf 1; Node[Leaf 2; Node[Leaf 3; Node[Leaf 4; Node[Leaf 5; Node[Leaf 6; Leaf 7]]]]]]

(* Exercise 2.26 *)
let xc = Node[Leaf 1; Leaf 2; Leaf 3]
let yc = Node[Leaf 4; Leaf 5; Leaf 6]
let rec append_tree tree1 tree2 =
   match tree1, tree2 with
    | Node x, Leaf y -> Node (x @ [Leaf y])
    | Leaf x, Node y -> Node ((Leaf x)::y)
    | Node x, Node y -> Node (x @ y)
    | Leaf x, Leaf y -> Node [Leaf x; Leaf y]
append_tree xc yc
Node[xc; Node[yc]]
Node[xc; yc]

(* Exercise 2.27 *)
let xd = Node[Node[Leaf 1; Leaf 2]; Node[Leaf 3; Leaf 4]]
let reverse'' tree =
   match tree with
    | Leaf x  -> Leaf x
    | Node xs -> Node (List.rev xs)
reverse'' xd
let rec deep_reverse tree =
   match tree with
    | Leaf x  -> Leaf x
    | Node xs -> Node (List.rev (map deep_reverse xs))
deep_reverse xd

(* Exercise 2.28 *)
let xe = Node[Node[Leaf 1; Leaf 2]; Node[Leaf 3; Leaf 4]]
let rec fringe tree =
   match tree with
    | Leaf x -> [x]
    | Node [] -> []
    | Node (x::xs) -> fringe x @ fringe (Node xs)
fringe xe
fringe(Node[xe; xe])

(* Exercise 2.29 *)
(* a. *)
let make_mobile left right = Node[left; right]
let make_branch length struc = Node[Leaf length; struc]
let left_branch tree =
   match tree with
    | Node[left; right] -> left
    | _ -> failwith "Error"
let right_branch tree =
   match tree with
    | Node[left; right] -> right
    | _ -> failwith "Error"
let branch_length tree =
   match tree with
    | Node[Leaf length; struc] -> length
    | _ -> failwith "Error"
let branch_structure tree =
   match tree with
    | Node[Leaf length; struc] -> struc
    | _ -> failwith "Error"
(* Remainder To Be Done *)

(* Exercise 2.30 *)
let rec square_tree tree =
   match tree with
    | Leaf x  -> Leaf (x*x)
    | Node xs -> Node(map square_tree xs)
square_tree
   (Node[Leaf 1;
      Node[Leaf 2; Node[Leaf 3; Leaf 4]; Leaf 5];
      Node[Leaf 6; Leaf 7]])

(* Exercise 2.31 *)
let rec tree_map proc tree =
   match tree with
    | Leaf x  -> Leaf(proc x)
    | Node xs -> Node(map (tree_map proc) xs)
let square_tree' tree = tree_map square tree
square_tree'(Node[Leaf 1;
              Node[Leaf 2; Node[Leaf 3; Leaf 4]; Leaf 5];
              Node[Leaf 6; Leaf 7]])

(* Exercise 2.32 *)
let rec subsets s =
   match s with
    | [] -> [[]]
    | x::xs ->
      let rest = subsets xs
      in rest @ (map (fun y -> x::y) rest)
subsets [1; 2; 3]

(* 2.2.3 Hierarchical Data and the Closure Property - Sequences as Conventional Interfaces *)

let isOdd' n = (n % 2 = 1)
let isEven' = compose not isOdd
let square' x = x * x

let rec sum_odd_squares tree =
   match tree with
    | Node [] -> 0
    | Leaf x when isOdd x-> square x
    | Leaf x -> 0
    | Node(x::xs) -> sum_odd_squares x + sum_odd_squares(Node xs)

let rec even_fibs n =
   let rec next k =
      if k > n
         then []
         else
            let f = fib k
            in
               if isEven f
                  then f::next(k+1)
                  else next(k+1)
   in next 0

(* Sequence operations *)
map square [1;2;3;4;5]

let rec filter' f = function
  | [] -> []
  | x::xs when f x -> x::filter' f xs
  | x::xs -> filter' f xs

filter' isOdd [1;2;3;4;5]

(* This is just List.fold_right *)
let rec accumulate f accu = function
  | [] -> accu
  | x::xs -> f x (accumulate f accu xs)

accumulate ( + ) 0 [1;2;3;4;5]
accumulate ( * ) 1 [1;2;3;4;5]
accumulate (fun x y -> x::y) [] [1;2;3;4;5]

let rec enumerate_interval low high =
   if low > high
      then []
      else low :: enumerate_interval (low+1) high

enumerate_interval 2 7

let rec enumerate_tree tree =
   match tree with
    | Node [] -> []
    | Leaf x -> [x]
    | Node (x::xs) -> enumerate_tree x @ enumerate_tree (Node xs)

enumerate_tree (Node[Leaf 1; Node[Leaf 2; Node[Leaf 3; Leaf 4]; Leaf 5]])

let sum_odd_squares' tree =
   accumulate (+) 0 (map square (filter isOdd (enumerate_tree tree)))

let even_fibs' n =
   accumulate (fun x y -> x::y) [] (filter isEven (map fib (enumerate_interval 0 n)))

let list_fib_squares n =
   accumulate (fun x y -> x::y) [] (map square (map fib (enumerate_interval 0 n)))

list_fib_squares 10

let product_of_squares_of_odd_elements sequence =
   accumulate ( * ) 1 (map square (filter isOdd sequence))

product_of_squares_of_odd_elements [1;2;3;4;5]

type employee = { name:string; jobtitle:string; salary:int }
let isProgrammer emp = (emp.jobtitle = "Programmer")
let salary emp = emp.salary
let salary_of_highest_paid_programmer records =
   accumulate (max) 0 (map salary (filter isProgrammer records))

let recs = [{name="Fred"; jobtitle="Programmer"; salary=180};
            {name="Hank"; jobtitle="Programmer"; salary=150}]
salary_of_highest_paid_programmer recs

(* Nested mappings *)
let n = 10                   (* book doesn't define n *)
accumulate (@) []
   (map
      (fun i -> map (fun j -> [i; j]) (enumerate_interval 1 (i-1)))
      (enumerate_interval 1 n))

let flatmap proc seq = accumulate (@) [] (map proc seq)

let rec has_no_divisors n c =
   c = 1 || (n % c <> 0 && has_no_divisors n (c-1))

let isPrime n = has_no_divisors n (n-1)

(* Unnecessary run-time checks can be replaced by lists with statically-known lengths (tuples). *)
let prime_sum (x, y) = isPrime (x + y)

let make_pair_sum (x, y) = (x, y, x+y)

let prime_sum_pairs n =
   map make_pair_sum
      (filter
         prime_sum
         (flatmap
            (fun i -> map (fun j -> i, j) (enumerate_interval 1 (i-1)))
            (enumerate_interval 1 n)))

let remove x list =
   filter (( <> ) x) list

let rec permutations = function
 | [] -> [[]]
 | s ->
      flatmap
         (fun x -> map (fun p -> x::p) (permutations (remove x s)))
         s

(* Exercise 2.34 *)
(* exercise left to reader to define appropriate functions
   let horner_eval x coefficient_sequence =
      accumulate (fun this_coeff higher_terms -> ??FILL_THIS_IN??) 0 coefficient_sequence
   horner_eval 2 [1;3;0;5;0;1] *)

(* Exercise 2.36 *)
(* exercise left to reader to define appropriate functions
   let accumulate_n oper init segs =
      if (segs = [])
         then []
         else
            (accumulate oper init ??FILL_THIS_IN??)::
            (accumulate_n oper init ??FILL_THIS_IN??)
   accumulate_n (+) 0 s *)

(* CMR Error - need to finish this one *)
(* Exercise 2.37 *
let dot_product(v, w) =
   accumulate
      op+
      0
      (map
            (fn i =>
               map
                  (fn j => i * j)
                  w)
            v)
let dot_product v w = fold_left ( + ) (List.map2 ( * ) v w)
*)

(* Exercise 2.38 *)
let fold_right = accumulate
let fold_left oper initial sequence =
   let rec iter result seq =
      match seq with
       | [] -> result
       | x::xs -> iter (oper result x) xs
   in iter initial sequence

fold_right (/) 1.0 [1.0;2.0;3.0]
fold_left (/) 1.0 [1.0;2.0;3.0]
fold_right (fun x y -> x::y) [] [1;2;3]
(* CMR Error - won't compile - Scheme result = (((() 1) 2) 3) *)
(* The result is not a valid list. -- jdh *)
(* fold_left (fun x y -> x::y) [] [1;2;3] *)

(* Exercise 2.42 *)
let na=8
let rec safe (x1, y1) (x2, y2) =
   x1 <> x2 && y1 <> y2 && x2 - x1 <> y2 - y1 && x1 - y2 <> x2 - y1
let rec search f n qs ps accu =
   match ps with
    | [] -> if length qs = n then f qs accu else accu
    | q::ps -> search f n (q::qs) (filter (safe q) ps) (search f n qs ps accu)
let rec init n f = if n=0 then [] else f(n-1) :: init (n-1) f
let ps = List.flatten (init na (fun i -> init na (fun j -> i, j)))
(* search (fun qs -> print qs; (+) 1) n 0 [] ps 0 *)
(* exercise left to reader to define appropriate functions
   let queens board_size =
      let queen_cols k =
         if (k = 0)
            then[empty_board]
            else
               filter
                  (fun positions -> isSafe k positions)
                  (flatmap
                     (fun rest_of_queens -> map
                        (fun new_row -> adjoin_position new_row k rest_of_queens)
                        (enumerate_interval 1 board_size))
                     (queen_cols (k-1)))
      in queen_cols board_size *)

(* Exercise 2.43 *)
(* exercise left to reader to define appropriate functions
   let queens board_size =
      let queen_cols k
         if (k = 0)
            then [empty_board]
            else
               filter
                  (fun positions -> isSafe k positions)
                  (flatmap
                     (fun new_row -> map
                        (fun rest_of_queens -> adjoin_position new_row k rest_of_queens)
                        (queen_cols (k-1)))
                     (enumerate_interval 1 board_size))
      in queen_cols board_size *)

(* 2.2.4 Hierarchical Data and the Closure Property - Example: a picture language *)

(* these two routines are to be written *)
let draw_line x y = ()
let wave xframe = xframe

type vect_rec = { x:float; y:float }
type vect = Vect of vect_rec

let make_vect x y = Vect{x=x; y=y}
let xcor_vect (Vect{x=x}) = x
let ycor_vect (Vect{y=y}) = y
let add_vect v1 v2 = make_vect (xcor_vect v1 + xcor_vect v2) (ycor_vect v1 + ycor_vect v2)
let sub_vect v1 v2 = make_vect (xcor_vect v1 - xcor_vect v2) (ycor_vect v1 - ycor_vect v2)
let scale_vect s v = make_vect (s * (xcor_vect v)) (s * (ycor_vect v))

type frame_rec = { origin:vect; edge1:vect; edge2:vect }
type frame = Frame of frame_rec
let make_frame origin edge1 edge2 = Frame{origin=origin; edge1=edge1; edge2=edge2}
let origin_frame (Frame{origin=origin}) = origin
let edge1_frame (Frame{edge1=edge1}) = edge1
let edge2_frame (Frame{edge2=edge2}) = edge2
let a_frame = make_frame (make_vect 0.0 0.0) (make_vect 1.0 0.0) (make_vect 0.0 1.0)

type 'a segment_record = { x:'a; y:'a }
type 'a segment = Segment of 'a segment_record
let start_segment (Segment{x=x}) = x
let end_segment (Segment{y=y}) = y

(* Frames *)
let frame_coord_map xframe v =
   add_vect
      (origin_frame xframe)
      (add_vect
         (scale_vect (xcor_vect v) (edge1_frame xframe))
         (scale_vect (ycor_vect v) (edge2_frame xframe)))

let _ = frame_coord_map a_frame (make_vect 0.0 0.0)
let _ = origin_frame a_frame

(* Painters *)
let rec foreach f items =
   match items with
    | [] -> ()
    | x::xs -> let _ = f x in foreach f xs

let segments_painter segment_list xframe =
   foreach
      (fun segment ->
         draw_line
            (frame_coord_map xframe (start_segment segment))
            (frame_coord_map xframe (end_segment segment)))
      segment_list

let transform_painter painter origin corner1 corner2 xframe =
   let m = frame_coord_map xframe in
   let new_origin = m origin
   in painter
         (make_frame
            new_origin
            (sub_vect (m corner1) new_origin)
            (sub_vect (m corner2) new_origin))

let flip_vert painter =
   transform_painter
      painter
      (make_vect 0.0 1.0)
      (make_vect 1.0 1.0)
      (make_vect 0.0 0.0)

let flip_horiz painter =
   transform_painter
      painter
      (make_vect 1.0 0.0)
      (make_vect 0.0 0.0)
      (make_vect 1.0 1.0)

let shrink_to_upper_right painter =
   transform_painter
      painter
      (make_vect 0.5 0.5)
      (make_vect 1.0 0.5)
      (make_vect 0.5 1.0)

let rotate90 painter =
   transform_painter
      painter
      (make_vect 1.0 0.0)
      (make_vect 1.0 1.0)
      (make_vect 0.0 0.0)

let rotate180 painter =
   transform_painter
      painter
      (make_vect 1.0 1.0)
      (make_vect 0.0 1.0)
      (make_vect 1.0 0.0)

let squash_inwards painter =
   transform_painter
      painter
      (make_vect 0.0 0.0)
      (make_vect 0.65 0.35)
      (make_vect 0.35 0.65)

let beside painter1 painter2 xframe =
   let split_point = make_vect 0.5 0.0 in
   let paint_left =
      transform_painter
         painter1
         (make_vect 0.0 0.0)
         split_point
         (make_vect 0.0 1.0)
   and paint_right =
      transform_painter
         painter2
         split_point
         (make_vect 1.0 0.0)
         (make_vect 0.5 1.0)
   in
      let _ = paint_left xframe
      in paint_right xframe

let below painter1 painter2 xframe =
   let split_point = make_vect 0.0 0.5 in
   let paint_below =
      transform_painter
         painter1
         (make_vect 0.0 0.0)
         (make_vect 1.0 0.0)
         split_point
   and paint_above =
      transform_painter
         painter2
         split_point
         (make_vect 1.0 0.5)
         (make_vect 0.0 1.0)
   in
      let _ = paint_below xframe
      in paint_above xframe

let rec up_split painter n =
   if n = 0
      then painter
      else
         let smaller = up_split painter (n-1)
         in below painter (beside smaller smaller)

let wave2 = beside wave (flip_vert wave)

let wave4 = below wave2 wave

let flipped_pairs painter =
   let painter2 = beside painter (flip_vert painter)
   in below painter2 painter2

let wave4' = flipped_pairs wave

let rec right_split painter n =
   if n = 0
      then painter
      else
         let smaller = right_split painter (n-1)
         in beside painter (below smaller smaller)

let rec corner_split painter n =
   if n = 0
      then painter
      else
         let up = up_split painter (n-1)
         and right = right_split painter (n-1) in
         let top_left = beside up up
         and bottom_right = below right right
         and corner = corner_split painter (n-1)
         in beside (below painter top_left) (below bottom_right corner)

let square_limit painter n =
   let quarter = corner_split painter n in
   let half = beside (flip_horiz quarter) quarter
   in below (flip_vert half) half

(* Higher_order operations *)
let square_of_four tleft tright bleft bright =
   fun painter ->
      let top = beside (tleft painter) (tright painter)
      and bottom = beside (bleft painter) (bright painter)
      in below bottom top

let flipped_pairs' painter =
   let combine4 = square_of_four identity flip_vert identity flip_vert
   in combine4 painter

(* footnote *)
(* CMR Error - let flipped_pairs'' = square_of_four identity flip_vert identity flip_vert *)

let square_limit' painter n =
   let combine4 = square_of_four flip_horiz identity rotate180 flip_vert
   in combine4 (corner_split painter n)

(* Exercise 2.45 *)
(* exercise left to reader to define appropriate functions
   let right_split = split beside below
   let up_split = split below beside *)

(* Exercise 2.47 *)
let make_frame' origin edge1 edge2 = [origin; edge1; edge2]
let make_frame'' origin edge1 edge2 = [origin; [edge1; edge2]]

(* 2.3.1 Symbolic Data - Quotation *)
let _ = ["a"; "b"; "c"; "d"]
let _ = [23; 45; 17]
type person_record = { name:string; age:int }
let _ = [{name="Norah"; age=12}; {name="Molly"; age=9}; {name="Anna"; age=7}; {name="Charlotte"; age=3}]

let xf = 1
let _ = (23 + 45) * (xf + 9)

let rec fact n =
   match n with
    | 1 -> 1
    | _ -> n * fact (n-1)

let a = 1
let b = 2
let _ = [a; b]

(* To Be Done *)

(* 2.3.2 Symbolic Data - Example: Symbolic Differentiation *)

(* representing algebraic expressions *)
type term = Number of int
          | Variable of char
          | Sum of term * term
          | Product of term * term

let isNumber = function
   | Number x -> true
   | _ -> false

let isSame_number = function
   | Number x, Number y -> (x = y)
   | _ -> false

let isVariable = function
   | Variable x -> true
   | _ -> false

let isSame_variable = function
   | Variable x, Variable y -> (x = y)
   | _ -> false

let isSum = function
   | Sum (x, y) -> true
   | _ -> false

let isProduct = function
   | Product (x, y) -> true
   | _ -> false

let make_sum = function
   | Number x, Number y -> Number (x + y)
   | x, y -> Sum (x, y)

let make_product = function
   | Number x, Number y -> Number (x * y)
   | x, y -> Product (x, y)

let addend = function
   | Sum (x, y) -> x
   | _ -> failwith "Error"

let augend = function
   | Sum (x, y) -> y
   | _ -> failwith "Error"

let multiplier = function
   | Product (x, y) -> x
   | _ -> failwith "Error"

let multiplicand = function
   | Product (x, y) -> y
   | _ -> failwith "Error"

let rec deriv exp var =
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
                  then make_sum(deriv (addend exp) var, deriv (augend exp) var)
                  else
                     if isProduct exp
                        then
                           make_sum(
                              make_product(multiplier exp, deriv (multiplicand exp) var),
                              make_product(deriv (multiplier exp) var, multiplicand exp))
                        else failwith "Error"

(* dx(x + 3) = 1 *)
let d1 = deriv (Sum(Variable 'x', Number 3)) (Variable 'x')

(* dx(x*y) = y *)
let d2 = deriv (Product(Variable 'x', Variable 'y')) (Variable 'x')

(* dx(x*y + x + 3) = y + 1 *)
let d3 = deriv (Sum(Sum(Product(Variable 'x', Variable 'y'), Variable 'x'), Number 3)) (Variable 'x')

(* With simplification *)
let make_sum' = function
   | Number 0, y -> y
   | x, Number 0 -> x
   | Number x, Number y -> Number (x + y)
   | x, y -> Sum (x, y)

let make_product' = function
   | Number 0, y -> Number 0
   | x, Number 0 -> Number 0
   | Number 1, y -> y
   | x, Number 1 -> x
   | Number x, Number y -> Number (x * y)
   | x, y -> Product (x, y)

let deriv' exp var =
   match exp, var with
    | Number x, var -> Number 0
    | Variable x, Variable y when x = y -> Number 1
    | Variable x, Variable y -> Number 0
    | Variable x, _ -> Number 0
    | Sum (x, y), _ -> make_sum'(deriv x var, deriv y var)
    | Product(x, y), var -> make_sum'(make_product'(x, deriv y var), make_product'(deriv x var, y))

(* dx(x + 3) = 1 *)
let d1' = deriv (Sum(Variable 'x', Number 3)) (Variable 'x')

(* dx(x*y) = y *)
let d2' = deriv (Product(Variable 'x', Variable 'y')) (Variable 'x')

(* dx(x*y + x + 3) = y + 1 *)
let d3' = deriv (Sum(Sum(Product(Variable 'x', Variable 'y'), Variable 'x'), Number 3)) (Variable 'x')

(* EXERCISE 2.57 *)
(* dx(x*y*(x+3)) = dx(x*x*y + x*y*3) = 2xy + 3y *)
(* exercise left to reader to define appropriate functions
   let d4 = derivx (Product(Product(Variable 'x', Variable 'y'), Sum(Variable 'x', Number 3))) (Variable 'x') *)

(* 2.3.3 Symbolic Data - Example: Representing Sets *)

(* unordered *)
let rec is_element_of_set x = function
   | [] -> false
   | y::ys when x = y -> true
   | y::ys -> is_element_of_set x ys

let adjoin_set x set =
   if is_element_of_set x set
      then set
      else x::set

let rec intersection_set set1 set2 =
   match set1, set2 with
    | [], _ -> []
    | _, [] -> []
    | x::xs, _ ->
      if is_element_of_set x set2
         then x::intersection_set xs set2
         else intersection_set xs set2

(* ordered *)
let rec is_element_of_set' x = function
   | [] -> false
   | y::ys when x = y -> true
   | y::ys when x < y -> false
   | y::ys -> is_element_of_set' x ys

let rec intersection_set' set1 set2 =
   match set1, set2 with
    | [], _ -> []
    | _, [] -> []
    | x::xs, y::ys when x = y -> x::intersection_set' xs ys
    | x::xs, y::ys when x < y -> intersection_set' xs set2
    | x::xs, y::ys -> intersection_set' set1 ys

(* binary trees *)
type 'a btree = Leaf
              | Node of 'a * 'a btree * 'a btree

let rec is_element_of_set'' x = function
   | Leaf -> false
   | Node(y, left, right) when x = y -> true
   | Node(y, left, right) when x < y -> is_element_of_set'' x left
   | Node(y, left, right) -> is_element_of_set'' x right

let _ = is_element_of_set'' 3 (Node(2, Node(1, Leaf, Leaf), Node(3, Leaf, Leaf)))

let rec adjoin_set' x sety =
   match sety with
    | Leaf -> Node(x, Leaf, Leaf)
    | Node(y, left, right) when x = y -> sety
    | Node(y, left, right) when x < y -> Node(y, adjoin_set' x left, right)
    | Node(y, left, right) -> Node(y, left, adjoin_set' x right)

adjoin_set' 3 (Node(4, Node(2, Leaf, Leaf), Node(6, Leaf, Leaf)))

(* Exercise 2.63 *)
let rec tree_to_list' tree =
   match tree with
    | Leaf -> []
    | Node(y, left, right) ->
      tree_to_list' left @ y::tree_to_list' right
let _ = tree_to_list'(Node(4, Node(2, Leaf, Leaf), Node(6, Leaf, Leaf)))

let tree_to_list'' tree =
   let rec copy_to_list t ys =
      match t with
       | Leaf -> ys
       | Node(x, left, right) -> copy_to_list left (x::copy_to_list right ys)
   in copy_to_list tree []
let _ = tree_to_list''(Node(4, Node(2, Leaf, Leaf), Node(6, Leaf, Leaf)))

(* Exercise 2.64 *)
let rec partial_tree elts n =
   match n with
    | 0 -> (Leaf, elts)
    | _ ->
      let left_size = (n - 1) / 2 in
      let right_size = n - (left_size + 1) in
      let left_result = partial_tree elts left_size in
      let (left_tree, non_left_elts) = left_result in
      let this_entry = List.hd non_left_elts in
      let right_result = partial_tree (List.tl non_left_elts) right_size in
      let (right_tree, remaining_elts)  = right_result
      in (Node(this_entry, left_tree, right_tree), remaining_elts)

let list_to_tree elements =
   let (result, _) = partial_tree elements (length elements)
   in result

let _ = list_to_tree [2; 4; 6]

(* information retrieval *)
type info_rec = { key:int; name:string; age:int }
type information = Information of info_rec
let rec lookup given_key = function
   | [] -> failwith "Error"
   | info::xs ->
      if given_key = info.key
         then info
         else lookup given_key xs

(* 2.3.4 Symbolic Data - Example: Huffman Encoding Trees *)

(* representing *)
type ('a, 'b) leaf_rec = { symbol:'a; weight:'b }
 and ('a, 'b) tree_rec = { left:('a, 'b) btree; right:('a, 'b) btree; symbols:'a list; weights:'b }
 and ('a, 'b) btree = Leaf of ('a, 'b) leaf_rec
                    | Tree of ('a, 'b) tree_rec

let make_leaf symbol weight = Leaf{ symbol=symbol; weight=weight }

let isLeaf = function
   | Leaf _ -> true
   | _ -> false

let symbol_leaf = function
   | Leaf x -> x.symbol
   | _ -> failwith "Error"

let weight_leaf = function
   | Leaf x -> x.weight
   | _ -> failwith "Error"

let symbols = function
   | Leaf x -> [x.symbol]
   | Tree x -> x.symbols

let weight = function
   | Leaf x -> x.weight
   | Tree x -> x.weights

let make_code_tree left right =
   Tree{ left = left;
         right = right;
         symbols = symbols left @ symbols right;
         weights = weight left + weight right }

let left_Node = function
   | Tree x -> x.left
   | _ -> failwith "Error"
let right_Node = function
   | Tree x -> x.right
   | _ -> failwith "Error"

let choose_Node n node =
   match n with
    | 0 -> left_Node node
    | 1 -> right_Node node
    | _ -> failwith "Error"

(* decoding *)
let decode bits tree =
   let rec decode' n1 n2 =
      match n1 with
       | [] -> []
       | x::xs ->
         let next_Node = choose_Node x n2
         in
            if isLeaf next_Node
               then symbol_leaf next_Node::decode' xs tree
               else decode' xs next_Node
   in decode' bits tree

(* sets *)
let rec adjoin_set'' x = function
   | [] -> [x]
   | y::ys ->
      if weight x < weight y
         then x::y::ys
         else y::adjoin_set'' x ys

let rec make_leaf_set = function
   | Leaf x::pairs ->
      adjoin_set (make_leaf x.symbol x.weight) (make_leaf_set pairs)
  | _ -> failwith "Error"

(* Exercise 2.67 *)
let sample_tree =
   make_code_tree
      (make_leaf 'A' 4)
      (make_code_tree
         (make_leaf 'B' 2)
         (make_code_tree
            (make_leaf 'D' 1)
            (make_leaf 'C' 1)))

let sample_message = [0; 1; 1; 0; 0; 1; 0; 1; 0; 1; 1; 1; 0]

let test = implode(decode sample_message sample_tree)

(* Exercise 2.68 *)
(* exercise left to reader to define appropriate functions
   let rec encode message tree =
      match message with
       | [] -> []
       | x::xs -> encode_symbol x tree @ encode xs tree *)

(* 2.4.1 Multiple Representations for Abstract Data - Representations for Complex Numbers *)
let square_float' x = x * x

(* Rectangular *)
let real_part z = List.hd z
let imag_part z = List.hd(List.tl z)

let magnitude z =
   sqrt(square_float (real_part z) + square_float (imag_part z))

let angle z =
   atan2 (imag_part z) (real_part z)

let make_from_float_imag x y = [x; y]
let make_from_mag_ang r a =
   [r * cos a; r * sin a]

module sicp_polar =
(* polar *)
   let magnitude (z : float list) = List.hd z
   let angle (z : float list) = List.hd(List.tl z)

   let real_part z =
      magnitude z * cos(angle z)

   let imag_part z =
      magnitude z * sin(angle z)

   let make_from_float_imag x y =
      [sqrt(square_float x + square_float y); atan2 y x]

   let make_from_mag_ang r a = [r; a]

(* using the abstract type *)
let z' = [1.0; 2.0]
make_from_float_imag (real_part z') (imag_part z')
make_from_mag_ang (magnitude z') (angle z')

let add_complex z1 z2 =
   make_from_float_imag
      (real_part z1 + real_part z2)
      (imag_part z1 + imag_part z2)

let sub_complex z1 z2 =
   make_from_float_imag
      (real_part z1 - real_part z2)
      (imag_part z1 - imag_part z2)

let mul_complex z1 z2 =
   make_from_mag_ang
      (magnitude z1 * magnitude z2)
      (angle z1 + angle z2)

let div_complex z1 z2 =
   make_from_mag_ang
      (magnitude z1 / magnitude z2)
      (angle z1 - angle z2)

(* 2.4.2 Multiple Representations for Abstract Data - Tagged Data *)

module sicp_intersection =
(* Using List with Intersection type *)
   type 'a tag = Rectangular | Polar | Contents of 'a

   let attach_tag type_tag contents = type_tag::contents

   let type_tag = function
    | Rectangular::_ -> Rectangular
    | Polar::_ -> Polar
    | _ -> failwith "Error"

   let contents = function
    | _::Contents x::[] -> x
    | _ -> failwith "Error"

   let isRectangular = function
    | Rectangular::_ -> true
    | _ -> false

   let isPolar = function
    | Polar::_ -> true
    | _ -> false

   (* Rectangular *)
   let make_from_float_imag_rectangular x y =
      [Rectangular; Contents x; Contents y]
   let make_from_mag_ang_rectangular r a =
      [Rectangular; Contents (r * cos a); Contents (r * sin a)]

   let real_part_rectangular = function
    | Rectangular::Contents x::_ -> x
    | _ -> failwith "Error"
   let imag_part_rectangular = function
    | Rectangular::_::Contents y::_ -> y
    | _ -> failwith "Error"

   let magnitude_rectangular z =
      sqrt(square_float (real_part_rectangular z) + square_float (imag_part_rectangular z))
   let angle_rectangular z =
      atan2 (imag_part_rectangular z) (real_part_rectangular z)

   (* Polar *)
   let make_from_float_imag_polar x y =
      [Polar; Contents (sqrt(square_float x + square_float y)); Contents (atan2 y x)]
   let make_from_mag_ang_polar r a =
      [Polar; Contents r; Contents a]

   let magnitude_polar = function
    | Polar::Contents x::_ -> x : float
    | _ -> failwith "Error"
   let angle_polar = function
    | Polar::_::Contents y::_ -> y : float
    | _ -> failwith "Error"

   let real_part_polar z =
      magnitude_polar z * cos(angle_polar z)
   let imag_part_polar z =
      magnitude_polar z * sin(angle_polar z)

   (* Generic selectors *)
   let real_part z =
      match z with
       | Rectangular::_ -> real_part_rectangular z
       | Polar::_ -> real_part_polar z
       | _ -> failwith "Error"
   let imag_part z =
      match z with
       | Rectangular::_ -> imag_part_rectangular z
       | Polar::_ -> imag_part_polar z
       | _ -> failwith "Error"

   let magnitude z =
      match z with
       | Rectangular::_ -> magnitude_rectangular z
       | Polar::_ -> magnitude_polar z
       | _ -> failwith "Error"
   let angle z =
      match z with
       | Rectangular::_ -> angle_rectangular z
       | Polar::_ -> angle_polar z
       | _ -> failwith "Error"

   (* same as before *)
   let add_complex z1 z2 =
      make_from_float_imag
         (real_part z1 + real_part z2)
         (imag_part z1 + imag_part z2)
   let sub_complex z1 z2 =
      make_from_float_imag
         (real_part z1 - real_part z2)
         (imag_part z1 - imag_part z2)
   let mul_complex z1 z2 =
      make_from_mag_ang
         (magnitude z1 * magnitude z2)
         (angle z1 + angle z2)
   let div_complex z1 z2 =
      make_from_mag_ang
         (magnitude z1 / magnitude z2)
         (angle z1 - angle z2)

   (* Constructors for complex numbers *)
   let make_from_float_imag x y =
      make_from_float_imag_rectangular x y
   let make_from_mag_ang r a =
      make_from_mag_ang_polar r a
(* End Using List with Intersection type *)

module sicp_records =
(* Using Records *)
   type 'a rectangular = { real_part : 'a; imag_part : 'a }
   type 'a polar = { magnitude : 'a; angle : 'a }
   type 'a tag = Rectangular of 'a rectangular
               | Polar of 'a polar

   let isRectangular' = function
     | Rectangular _ -> true
     | _ -> false

   let isPolar' = function
    | Polar _ -> true
    | _ -> false

   (* Rectangular *)
   let make_from_float_imag_rectangular x y =
      Rectangular{ real_part = x; imag_part = y }
   let make_from_mag_ang_rectangular r a =
      Rectangular{ real_part = r * cos a; imag_part = r * sin a }

   let real_part_rectangular = function
    | Rectangular x -> x.real_part
    | _ -> failwith "Error"
   let imag_part_rectangular = function
    | Rectangular y -> y.imag_part
    | _ -> failwith "Error"

   let magnitude_rectangular z =
      sqrt(square_float(real_part_rectangular z) + square_float(imag_part_rectangular z))
   let angle_rectangular z =
      atan2 (imag_part_rectangular z) (real_part_rectangular z)

   (* Polar *)
   let make_from_float_imag_polar x y =
      Polar { magnitude = sqrt(square_float x + square_float y); angle = atan2 y x }
   let make_from_mag_ang_polar r a =
      Polar { magnitude = r; angle = a }

   let magnitude_polar = function
     | Polar x -> x.magnitude : float
     | _ -> failwith "Error"
   let angle_polar = function
     | Polar y -> y.angle
     | _ -> failwith "Error"

   let real_part_polar z =
      magnitude_polar z * cos(angle_polar z)
   let imag_part_polar z =
      magnitude_polar z * sin(angle_polar z)

   (* Generic selectors *)
   let real_part z =
      match z with
       | Rectangular _ -> real_part_rectangular z
       | Polar _ -> real_part_polar z
   let imag_part z =
      match z with
       | Rectangular _ -> imag_part_rectangular z
       | Polar _ -> imag_part_polar z

   let magnitude z =
      match z with
       | Rectangular _ -> magnitude_rectangular z
       | Polar _ -> magnitude_polar z
   let angle z =
      match z with
       | Rectangular _ -> angle_rectangular z
       | Polar _ -> angle_polar z

   (* same as before *)
   let add_complex z1 z2 =
      make_from_float_imag
         (real_part z1 + real_part z2)
         (imag_part z1 + imag_part z2)
   let sub_complex z1 z2 =
      make_from_float_imag
         (real_part z1 - real_part z2)
         (imag_part z1 - imag_part z2)
   let mul_complex z1 z2 =
      make_from_mag_ang
         (magnitude z1 * magnitude z2)
         (angle z1 + angle z2)
   let div_complex z1 z2 =
      make_from_mag_ang
         (magnitude z1 / magnitude z2)
         (angle z1 - angle z2)

   (* Constructors for complex numbers *)
   let make_from_float_imag x y =
      make_from_float_imag_rectangular x y
   let make_from_mag_ang r a =
      make_from_mag_ang_polar r a
(* End Using Records *)

(* 2.4.3 Multiple Representations for Abstract Data - Data-Directed Programming and Additivity *)

(* To Be Done *)
