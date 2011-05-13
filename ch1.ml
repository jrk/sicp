(* SICP Chapter #01 Examples in O'Caml *)
(* 1.1.1 The Elements of Programming - Expressions *)
486;;
137 + 349;;
1000 - 334;;
5 * 99;;
10 / 5;;
2.7 +. 10.0;;
21 + 35 + 12 + 7;;
25 * 4 * 12;;
3 * 5 + 10 - 6;;
3 * (2 * 4 + 3 + 5) + 10 - 7 + 6;;

(* 1.1.2 The Elements of Programming - Naming and the Environment *)
let size' = 2;;
size';;
5 * size';;
let pi = 3.14159
let radius = 10.0;;
pi *. radius *. radius;;
let circumference = 2.0 *. pi *. radius;;
circumference;;

(* 1.1.3 The Elements of Programming - Evaluating Combinations *)
(2 + 4 * 6) * (3 + 5 + 7);;

(* 1.1.4 The Elements of Programming - Compound Procedures *)
let square x = x * x;;
square 21;;
square(2 + 5);;
square(square 3);;
let sum_of_squares x y = square x + square y;;
sum_of_squares 3 4;;
let f a = sum_of_squares (a + 1) (a * 2);;
f 5;;

(* 1.1.5 The Elements of Programming - The Substitution Model for Procedure Application *)
f 5;;
sum_of_squares (5 + 1) (5 * 2);;
square 6 + square 10;;
6 * 6 + 10 * 10;;
36 + 100;;
f 5;;
sum_of_squares (5 + 1) (5 * 2);;
square(5 + 1) + square(5 * 2);;

((5 + 1) * (5 + 1)) + ((5 * 2) * (5 * 2));;
(6 * 6) + (10 * 10);;
36 + 100;;
136;;

(* 1.1.6 The Elements of Programming - Conditional Expressions and Predicates *)
let abs' x =
   if x > 0 then x
   else if x = 0 then 0
   else -x;;
let abs' x =
   if x < 0
      then -x
      else x
let x = 6;;
x > 5 && x < 10;;
let ge x y = x > y || x = y
let ge x y = not(x < y);;

(* Exercise 1.1 *)
10;;
5 + 3 + 4;;
9 - 1;;
6 / 2;;
2 * 4 + 4 - 6;;
let a = 3
let b = a + 1;;
a + b + a * b;;
a = b;;
if b > a && b < a * b
   then b
   else a;;
if a = 4
   then 6
   else
      if b = 4
         then 6 + 7 + a
         else 25;;
2 + if b > a then b else a;;
(if a > b then a
 else if a < b then b
 else -1) * (a + 1);;

(* Exercise 1.2 *)
(5. +. 4. +. (2. -. (3. -. (6. +. 4. /. 5.)))) /.
   (3. *. (6. -. 2.) *. (2. -. 7.));;

(* Exercise 1.3 *)
let three_n n1 n2 n3 =
   if n1 > n2
      then
         if n1 > n3
            then
               if n2 > n3
                  then n1*n1 + n2*n2
                  else n1*n1 + n3*n3
            else n1*n1 + n3*n3
      else
         if n2 > n3
            then
               if n1 > n3
                  then n2*n2 + n1*n1
                  else n2*n2 + n3*n3
            else n2*n2 + n3*n3

(* Exercise 1.4 *)
let a_plus_abs_b a b =
   if b > 0
      then a + b
      else a - b

(* Exercise 1.5 *)
let rec p () = p()
let test x y =
   if x = 0
      then 0
      else y;;
(* commented out as this is in infinite loop
   test 0 p();;
*)

(* 1.1.7 The Elements of Programming - Example: Square Roots by Newton's Method *)
let abs_float' x =
   if (x < 0.0)
      then -. x
      else x

let square_real x = x *. x

let good_enough guess x =
   abs_float'(square_real guess -. x) < 0.001

let average x y =
   (x +. y) /. 2.0

let improve guess x =
   average guess (x /. guess)

let rec sqrt_iter guess x =
   if good_enough guess x
      then guess
      else sqrt_iter (improve guess x) x

let sqrt' x =
   sqrt_iter 1.0 x;;

sqrt' 9.0;;
sqrt'(100.0 +. 37.0);;
sqrt'(sqrt' 2.0 +. sqrt' 3.0);;
square_real(sqrt' 1000.0);;

(* Exercise 1.6 *)
let new_if predicate then_clause else_clause =
   if predicate
      then then_clause
      else else_clause;;
new_if (2=3) 0 5;;
new_if (1=1) 0 5;;
let rec sqrt_iter guess x =
   new_if
      (good_enough guess x)
      guess
      (sqrt_iter (improve guess x) x)

(* from wadler paper *)
let newif p x y =
   match p with
    | true  -> x
    | false -> y

(* Exercse 1.7 *)
let good_enough_gp guess prev =
   abs_float'(guess -. prev) /. guess < 0.001

let rec sqrt_iter_gp guess prev x =
   if good_enough_gp guess prev
      then guess
      else sqrt_iter_gp (improve guess x) guess x

let sqrt_gp x =
   sqrt_iter_gp 4.0 1.0 x

(* Exercise 1.8 *)
let improve_cube guess x =
   (2.0 *. guess +. x /. (guess *. guess)) /. 3.0

let rec cube_iter guess prev x =
   if good_enough_gp guess prev
      then guess
      else cube_iter (improve_cube guess x) guess x

let cube_root_0 x =
   cube_iter 27.0 1.0 x

(* 1.1.8 The Elements of Programming - Procedures as Black-Box Abstractions *)
let square_real x = x *. x

let double x = x +. x

let square_real x = exp(double(log x))

let good_enough guess x =
   abs_float'(square_real guess -. x) < 0.001

let improve guess x =
   average guess (x /. guess)

let rec sqrt_iter guess x =
   if good_enough guess x
      then guess
      else sqrt_iter (improve guess x) x

let sqrt' x =
   sqrt_iter 1.0 x;;

square_real 5.0;;

(* Block-structured *)
let sqrt' x =
   let good_enough guess x =
      abs_float'(square_real guess -. x) < 0.001

   and improve guess x =
      average guess (x /. guess) in

   let rec sqrt_iter guess x =
      if good_enough guess x
         then guess
         else sqrt_iter (improve guess x) x

   in sqrt_iter 1.0 x

(* Taking advantage of lexical scoping *)
let sqrt' x =
   let good_enough guess =
      abs_float'(square_real guess -. x) < 0.001

   and improve guess =
      average guess (x /. guess) in

   let rec sqrt_iter guess =
      if good_enough guess
         then guess
         else sqrt_iter (improve guess)

   in sqrt_iter 1.0

(* 1.2.1 Procedures and the Processes They Generate - Linear Recursion and Iteration *)

(* Recursive *)
let rec factorial n =
   if n = 1
      then 1
      else n * factorial(n - 1);;

factorial 6;;

(* Iterative *)
let rec fact_iter product counter max_count =
   if counter > max_count
      then product
      else fact_iter (counter * product) (counter + 1) max_count

let factorial n =
   fact_iter 1 1 n

(* Iterative, block-structured (from footnote) *)
let factorial n =
   let rec iter product counter =
      if counter > n
         then product
         else iter (counter * product) (counter + 1)
   in iter 1 1

(* Exercise 1.9 *)
let inc a = a + 1
let dec a = a - 1
let rec plus a b =
   if a = 0
      then b
      else inc(plus (dec a) b)
let rec plus a b =
   if a = 0
      then b
      else plus (dec a) (inc b)

(* Exercise 1.10 *)
let rec a x y =
   match x, y with
    | x, 0 -> 0
    | 0, y -> 2 * y
    | x, 1 -> 2
    | x, y -> (a (x - 1) (a x (y - 1)));;
a 1 10;;
a 2 4;;
a 3 3;;
let f n = a 0 n
let g n = a 1 n
let h n = a 2 n
let k n = 5 * n * n

(* 1.2.2 Procedures and the Processes They Generate - Tree Recursion *)

(* Recursive *)
let rec fib n =
   match n with
    | 0 -> 0
    | 1 -> 1
    | n -> fib(n - 1) + fib(n - 2)

(* Iterative *)
let rec fib_iter a b count =
   match count with
    | 0 -> b
    | count -> fib_iter (a + b) a (count - 1)
let fib n =
   fib_iter 1 0 n

(* Counting change *)
let first_denomination x =
   match x with
    |  1 -> 1
    | 2 -> 5
    | 3 -> 10
    | 4 -> 25
    | 5 -> 50
    | x -> raise Not_found

let rec cc amount kinds_of_coins =
   if amount = 0 then 1
   else if amount < 0 then 0
   else if kinds_of_coins = 0 then 0
   else (cc amount (kinds_of_coins - 1)) +
        (cc (amount - (first_denomination kinds_of_coins)) kinds_of_coins)

let count_change amount =
   cc amount 5;;

count_change 100;;

(* Exercise 1.11 *)
let rec f n =
   if n < 3
      then n
      else f(n-1) + 2*f(n-2) + 3*f(n-3)

let rec f_iter a b c count =
   match count with
    | 0 -> c
    | _ -> f_iter (a + 2*b + 3*c) a b (count-1)

let f n = f_iter 2 1 0 n

(* Exercise 1.12 *)
let rec pascals_triangle n k =
   match n, k with
    | 0, k -> 1
    | n, 0 -> 1
    | n, k ->
         if n = k
            then 1
            else (pascals_triangle (n-1) (k-1)) + (pascals_triangle (n-1) k)

(* 1.2.3 Procedures and the Processes They Generate - Orders of Growth *)

(* Exercise 1.15 *)
let cube x = x *. x *. x
let p x = (3.0 *. x) -. (4.0 *. cube x)
let rec sine angle =
   if not(abs_float' angle > 0.1)
      then angle
      else p(sine(angle /. 3.0))

(* 1.2.4 Procedures and the Processes They Generate - Exponentiation *)

(* Linear recursion *)
let rec expt b n =
   match n with
    | 0 -> 1
    | n -> b * (expt b (n - 1))

(* Linear iteration *)
let rec expt_iter b counter product =
   match counter with
    | 0 -> product
    | counter -> expt_iter b (counter - 1) (b * product)
let expt b n =
   expt_iter b n 1

(* Logarithmic iteration *)
let even n = ((n mod 2) = 0)

let rec fast_expt b n =
   match n with
    | 0 -> 1
    | n ->
         if even n
            then square(fast_expt b (n / 2))
            else b * (fast_expt b (n - 1))

(* Exercise 1.17 *)
let multiply a b =
   match b with
    | 0 -> 0
    | b -> a + a*(b - 1)

(* Exercise 1.19 *)
let rec fib_iter a b p q count =
   match count with
    | 0 -> b
    | count ->
         if even count
            then fib_iter a b (p*p + q*q) (2*p*q + q*q) (count / 2)
            else fib_iter (b*q + a*q + a*p) (b*p + a*q) p q (count - 1)
let fib n =
   fib_iter 1 0 0 1 n

(* 1.2.5 Procedures and the Processes They Generate - Greatest Common Divisors *)
let rec gcd a b =
   match b with
    | 0 -> a
    | b -> gcd b (a mod b);;

gcd 40 6;;

(* Exercise 1.20 *)
gcd 206 40;;

(* 1.2.6 Procedures and the Processes They Generate - Example: Testing for Primality *)

(* prime *)
let divides a b = (b mod a = 0)

let rec find_divisor n test_divisor =
   if square test_divisor > n then n
   else if divides test_divisor n then test_divisor
   else find_divisor n (test_divisor + 1)

let smallest_divisor n = find_divisor n 2

let prime n = (n = smallest_divisor n)

(* fast_prime *)
let rec expmod nbase nexp m =
   match nexp with
    | 0 -> 1
    | nexp ->
         if even nexp
            then square(expmod nbase (nexp / 2) m) mod m
            else (nbase * (expmod nbase (nexp - 1) m)) mod m

let fermat_test n =
   let try_it a = ((expmod a n n) = a)
   in try_it(1 + Random.int(n - 1))

let rec fast_prime n ntimes =
   match ntimes with
    | 0 -> true
    | ntimes ->
         if fermat_test n
            then fast_prime n (ntimes - 1)
            else false;;

(* Exercise 1.21 *)
smallest_divisor 199;;
smallest_divisor 1999;;
smallest_divisor 19999;;

(* Exercise 1.22 *)
let report_prime elapsed_time =
   print_string (" *** " ^ (string_of_float elapsed_time))

let start_prime_test n start_time =
   if (prime n)
      then report_prime(Sys.time() -. start_time)
      else ()

let timed_prime_test n =
   let x = print_string ("\n" ^ (string_of_int n))
   in start_prime_test n (Sys.time())

(* Exercise 1.25 *)
let expmod nbase nexp m =
   (fast_expt nbase nexp) mod m

(* Exercise 1.26 *)
let rec expmod nbase nexp m =
   match nexp with
    | 0 -> 1
    | nexp ->
         if (even nexp)
            then ((expmod nbase (nexp / 2) m) * (expmod nbase (nexp / 2) m)) mod m
            else (nbase * (expmod nbase (nexp - 1) m)) mod m

(* Exercise 1.27 *)
let carmichael n =
   (fast_prime n 100) && not(prime n);;

carmichael 561;;
carmichael 1105;;
carmichael 1729;;
carmichael 2465;;
carmichael 2821;;
carmichael 6601;;

(* 1.3 Formulating Abstractions with Higher-Order Procedures *)
let cube x = x * x * x

(* 1.3.1 Formulating Abstractions with Higher-Order Procedures - Procedures as Arguments *)
let rec sum_integers a b =
   if a > b
      then 0
      else a + (sum_integers (a + 1) b)

let rec sum_cubes a b =
   if a > b
      then 0
      else cube a + (sum_cubes (a + 1) b)

let rec pi_sum a b =
   if a > b
      then 0.0
      else (1.0 /. (a *. (a +. 2.0))) +. (pi_sum (a +. 4.0) b)

let rec sum term a next b =
   if a > b
      then 0
      else term a + (sum term (next a) next b)


(* Using sum *)
let inc n = n + 1

let sum_cubes a b =
   sum cube a inc b;;

sum_cubes 1 10;;

let identity x = x

let sum_integers a b =
   sum identity a inc b;;

sum_integers 1 10;;

let rec sum_real term a next b =
   if a > b
      then 0.0
      else term a +. (sum_real term (next a) next b)

let pi_sum a b =
   let pi_term x = 1.0 /. (x *. (x +. 2.0))
   and pi_next x = x +. 4.0
   in sum_real pi_term a pi_next b;;

8.0 *. (pi_sum 1.0 1000.0);;

let integral f a b dx =
   let add_dx x = x +. dx
   in (sum_real f (a +. (dx /. 2.0)) add_dx b) *. dx

let cube_real x = x *. x *. x;;

integral cube_real 0.0 1.0 0.01;;
integral cube_real 0.0 1.0 0.001;;

(* Exercise 1.29 *)
let simpson f a b n =
   let h = abs_float(b -. a) /. (float_of_int n) in
   let rec sum_iter term start next stop acc =
      if start > stop
         then acc
         else sum_iter term (next start) next stop (acc +. (term (a +. (float_of_int start) *. h)))
   in h *. (sum_iter f 1 inc n 0.0);;

simpson cube_real 0.0 1.0 100;;

(* Exercise 1.30 *)
let rec sum_iter term a next b acc =
   if a > b
      then acc
      else sum_iter term (next a) next b (acc + term a)
let sum_cubes a b =
   sum_iter cube a inc b 0;;
sum_cubes 1 10;;

(* Exercise 1.31 *)
let rec product term a next b =
   if a > b
      then 1
      else term a * (product term (next a) next b)
let factorial n =
   product identity 1 inc n

let rec product_iter term a next b acc =
   if a > b
      then acc
      else product_iter term (next a) next b (acc * term a)

(* Exercise 1.32 *)
let rec accumulate combiner null_value term a next b =
   if a > b
      then null_value
      else combiner (term a) (accumulate combiner null_value term (next a) next b)
let sum a b = accumulate ( + ) 0 identity a inc b
let product a b = accumulate ( * ) 1 identity a inc b

let rec accumulate_iter combiner term a next b acc =
   if a > b
      then acc
      else accumulate_iter combiner term (next a) next b (combiner acc (term a))
let sum a b = accumulate_iter ( + ) identity a inc b 0
let product a b = accumulate_iter ( * ) identity a inc b 1

(* Exercise 1.33 *)
let rec filtered_accumulate combiner null_value term a next b pred =
   if a > b
      then null_value
      else
         if pred a
            then combiner (term a) (filtered_accumulate combiner null_value term (next a) next b pred)
            else filtered_accumulate combiner null_value term (next a) next b pred;;
filtered_accumulate ( +) 0 square 1 inc 5 prime;;

(* 1.3.2 Formulating Abstractions with Higher-Order Procedures - Constructing Procedures Using Lambda *)
let pi_sum a b =
   sum_real (fun x -> 1.0 /. (x *. (x +. 2.0))) a (fun x -> x +. 4.0) b

let integral f a b dx =
   (sum_real f (a +. (dx /. 2.0)) (fun x -> x +. dx) b) *. dx

let plus4 x = x + 4

let plus4 = fun x -> x + 4;;

(fun x y z -> x + y + (square z)) 1 2 3;;

(* Using let *)
let f x y =
   let f_helper a b =
      (x * (square a)) + (y * b) + (a * b)
   in f_helper (1 + (x * y)) (1 - y)

let f x y =
   (fun a b -> (x * square a) + (y * b) + (a * b))
      (1 + x*y) (1 - y)

let f x y =
   let a = 1 + x*y
   and b = 1 - y
   in (x * square a) + y*b + a*b

let x = 5;;
let x = 3
in
   x + (x * 10); + x;;

let x = 2;;
let x = 3
and y = x + 2
in
   x * y;;

let f x y =
   let a = 1 + x*y
   and b = 1 - y
   in x + square a + y*b + a*b

(* Exercise 1.34 *)
let f g = g 2;;
f square;;
f(fun z -> z * (z + 1));;

(* 1.3.3 Formulating Abstractions with Higher-Order Procedures - Procedures as General Methods *)

(* Half-interval method *)
let close_enough x y =
   (abs_float'(x -. y) < 0.001)

let positive x = (x >= 0.0)
let negative x = not(positive x)

let rec search f neg_point pos_point =
   let midpoint = average neg_point pos_point
   in
      if close_enough neg_point pos_point
         then midpoint
         else
            let test_value = f midpoint
            in
               if positive test_value      then search f neg_point midpoint
               else if negative test_value then search f midpoint pos_point
               else midpoint

exception Invalid of string;;
let half_interval_method f a b =
   let a_value = f a
   and b_value = f b
   in
      if negative a_value && positive b_value      then (search f a b)
      else if negative b_value && positive a_value then (search f b a)
      else raise (Invalid("Values are not of opposite sign" ^ string_of_float a ^ " " ^ string_of_float b));;

half_interval_method sin 2.0 4.0;;

half_interval_method (fun x -> (x *. x *. x) -. (2.0 *. x) -. 3.0) 1.0 2.0;;

(* Fixed points *)
let tolerance = 0.00001

let fixed_point f first_guess =
   let close_enough v1 v2 =
      abs_float'(v1 -. v2) < tolerance in
   let rec tryme guess =
      let next = f guess
      in
         if close_enough guess next
            then next
            else tryme next
   in tryme first_guess;;

fixed_point cos 1.0;;

fixed_point (fun y -> sin y +. cos y) 1.0;;

(* note: this function does not converge *)
let sqrt' x =
   fixed_point (fun y -> x /. y) 1.0

let sqrt' x =
   fixed_point (fun y -> (average y  (x /. y))) 1.0;;

(* Exercise 1.35 *)
let goldenRatio () =
   fixed_point (fun x -> 1.0 +. 1.0 /. x) 1.0;;

(* Exercise 1.36 *)
(* 35 guesses before convergence *)
fixed_point (fun x -> log 1000.0 /. log x) 1.5;;
(* 11 guesses before convergence (AverageDamp defined below) *)
(* fixed_point (average_damp (fun x -> log 1000.0 /. log x)) 1.5;; *)

(* Exercise 1.37 *)
(* exercise left to reader to define cont_frac
   cont_frac (fun i -> 1.0) (fun i -> 1.0) k;;
*)

(* 1.3.4 Formulating Abstractions with Higher-Order Procedures - Procedures as Returned Values *)
let average_damp f x = average x (f x);;

average_damp square_real 10.;;

let sqrt' x =
   fixed_point (average_damp (( /. ) x)) 1.

let cube_root x =
   fixed_point (average_damp (fun y -> x /. square_real y)) 1.

(* Newton's method *)
let dx = 0.00001
let deriv g x = (g(x +. dx) -. g x) /. dx

let cube x = x *. x *. x;;

(deriv cube) 5.;;

let newton_transform g x =
   x -. g x /. deriv g x

let newtons_method g guess =
   fixed_point (newton_transform g) guess

let sqrt' x =
   newtons_method (fun y -> (square_real y) -. x) 1.

(* Fixed point of transformed function *)
let fixed_point_of_transform g transform guess =
   fixed_point (transform g) guess

let sqrt' x =
   fixed_point_of_transform (( /. ) x) average_damp 1.

let sqrt' x =
   fixed_point_of_transform(fun y -> square_real y -. x) newton_transform 1.;;

(* Exercise 1.40 *)
let cubic a b c =
   fun x -> cube x +. (a *. x *. x) +. (b *. x) +. c;;
newtons_method (cubic 5.0 3.0 2.5) 1.0;;

(* Exercise 1.41 *)
let double_ f =
   fun x -> f(f x);;
(double_ inc)(5);;
((double_ double_) inc)(5);;
((double_ (double_ double_)) inc)(5);;

(* Exercise 1.42 *)
let compose f g =
   fun x -> f(g x);;
(compose square inc) 6;;

(* Exercise 1.43 *)
let repeated f n =
   let rec iterate arg i =
      if i > n
         then arg
         else iterate (f arg) (i+1)
   in fun x -> iterate x 1;;
(repeated square 2) 5;;

(* Exercise 1.44 *)
let smooth f dx =
   fun x -> average x ((f(x -. dx) +. f(x) +. f(x +. dx)) /. 3.0);;
fixed_point (smooth (fun x -> log 1000.0 /. log x) 0.05) 1.5;;

(* Exercise 1.46 *)
let iterative_improve good_enough improve =
   let rec iterate guess =
      let next = improve guess
      in
         if good_enough guess next
            then next
            else iterate next
   in fun x -> iterate x
let fixed_point f first_guess =
   let tolerance = 0.00001
   and good_enough v1 v2 = abs_float(v1 -. v2) < tolerance
   in (iterative_improve good_enough f) first_guess;;
fixed_point (average_damp (fun x -> log 1000.0 /. log x)) 1.5;;
