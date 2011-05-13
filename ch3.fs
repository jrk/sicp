(* SICP Chapter #03 Examples in F# *)
#light
(* Functions defined in previous chapters *)
let rec gcd a = function
   | 0 -> a
   | b -> gcd b (a % b)
let square_real x = x * x
let average x y = (x + y) / 2.0
let rec has_no_divisors n c =
   c = 1 || (n % c <> 0 && has_no_divisors n (c-1))
let isPrime n = has_no_divisors n (n-1)
let rec enumerate_interval low high =
   if low > high
      then []
      else low :: enumerate_interval (low+1) high
let compose f g x = f(g x)
let isOdd n = (n % 2 = 1)
let isEven = compose not isOdd

(* 3.1.1 - Assignment and Local State - Local State Variables *)
let balance = ref 100

exception InsufficientFunds of int

let withdraw amount =
   if !balance >= amount
      then begin balance := (!balance - amount); !balance end
      else raise (InsufficientFunds (!balance))

let _ = withdraw 25
let _ = withdraw 25
let _ = try withdraw 60 with InsufficientFunds b -> b
let _ = withdraw 15

let new_withdraw =
   let balance = ref 100
   in fun amount ->
      if !balance >= amount
         then begin balance := (!balance - amount); !balance end
         else raise (InsufficientFunds (!balance))

let make_withdraw balance =
   let balance = ref balance
   in fun amount ->
      if !balance >= amount
         then begin balance := (!balance - amount); !balance end
         else raise (InsufficientFunds (!balance))

let w1 = make_withdraw 100
let w2 = make_withdraw 100

let _ = w1 50
let _ = w2 70
let _ = try w2 40 with InsufficientFunds b -> b
let _ = w1 40

type ('a, 'b, 'c) account_rec = { withdraw:'a; deposit:'b; balance:'c }

let make_account balance =
   let balance = ref balance in
   let withdraw amount =
      if !balance >= amount
         then begin balance := (!balance - amount); !balance end
         else raise (InsufficientFunds (!balance))
   and deposit amount =
      balance := (!balance + amount); !balance
   and getbalance = !balance
   in { withdraw=withdraw; deposit=deposit; balance=getbalance }

let acc = make_account 100
let _ = acc.withdraw 50
let _ = try acc.withdraw 60  with InsufficientFunds b -> b
let _ = acc.deposit 40
let _ = acc.withdraw 60

let acc2 = make_account 100

(* Exercise 3.1 *)
(* exercise left to reader to define appropriate functions
   let a = make_accumulator 5
   let _ = a.f 10
   let _ = a.f 10 *)

(* Exercise 3.2 *)
(* exercise left to reader to define appropriate functions
   let s = make_monitored sqrt
   let _ = s.f 100
   let _ = s.how_many_calls() *)

(* Exercise 3.3 *)
(* exercise left to reader to define appropriate functions
   let acc = make_account 100 "secret-password"
   let _ = acc.withdraw 40 "secret-password"
   let _ = acc.withdraw 50 "some-other-password" *)

(* 3.1.2 - Assignment and Local State - The Benefits of Introducing Assignment *)
let random_init = ref 7

let rand_update x =
   let a = 27
   and b = 26
   and m = 127
   in (a * x + b) % m

let rand =
   let x = random_init
   in fun () -> x := (rand_update !x); !x

let cesaro_test () = gcd (rand()) (rand()) = 1

let monte_carlo trials experiment =
   let rec iter trials_remaining trials_passed =
      match trials_remaining with
       | 0 -> float_of_int trials_passed / float_of_int trials
       | _ ->
         if experiment()
            then iter (trials_remaining - 1) (trials_passed + 1)
            else iter (trials_remaining - 1) trials_passed
   in iter trials 0

let estimate_pi trials = sqrt (6. / (monte_carlo trials cesaro_test))

(* second version (no assignment) *)
let random_gcd_test trials initial_x =
   let rec iter x trials_passed =
      let x1 = rand_update x in
      let x2 = rand_update x1
      in function
         | 0 -> (float trials_passed) / (float trials)
         | trials_remaining ->
            iter  x2
               (if gcd x1 x2 = 1 then (trials_passed + 1) else trials_passed)
               (trials_remaining - 1)
   in iter initial_x 0 trials

(* alternate translation *)
let random_gcd_test' trials initial_x =
   let rec iter trials_remaining trials_passed x =
      let x1 = rand_update x in
      let x2 = rand_update x1
      in
         if trials_remaining = 0
            then float_of_int trials_passed / float_of_int trials
            else
               if gcd x1 x2 = 1
                  then iter (trials_remaining - 1) (trials_passed + 1) x2
                  else iter (trials_remaining - 1) trials_passed x2
   in iter trials 0 initial_x

let estimate_pi' trials = sqrt (6. / (random_gcd_test' trials !random_init))

(* Exercise 3.6 *)
(* exercise left to reader to define appropriate functions
   fun random_in_range low high =
      let range = high - low
      in low + random range
      end *)

(* 3.1.3 - Assignment and Local State - The Cost of Introducing Assignment *)
let make_simplified_withdraw balance =
   let balance = ref balance in
   fun amount ->
      balance := !balance - amount;
      !balance

let w = make_simplified_withdraw 25
let _ = w 20
let _ = w 10

let make_decrimenter balance =
   fun amount -> balance - amount

let d = make_decrimenter 25
let _ = d 20
let _ = d 10

let _ = (make_decrimenter 25) 20
let _ = (fun amount -> 25 - amount) 20
let _ = 25 - 20

let _ = (make_simplified_withdraw 25) 20
(* we add an additional step here to handle the introduction of the balance ref *)
let _ = (let balance = ref 25 in fun amount -> balance := !balance - amount) 20
let _ = (fun amount -> balance := 25 - amount) 20
let _ = (balance := 25 - 20)

(* Sameness and change *)
let d2 = make_decrimenter 25
let d2' = make_decrimenter 25

let w1' = make_simplified_withdraw 25
let w2' = make_simplified_withdraw 25
let _ = w1' 20
let _ = w1' 20
let _ = w2' 20

let peter_acc = make_account 100
let paul_acc = make_account 100

let peter_acc' = make_account 100
let paul_acc' = peter_acc

(* Pitfalls of imperative programming *)
let factorial n =
   let rec iter product counter =
      if counter > n
         then product
         else iter (counter * product) (counter + 1)
   in iter 1 1

let factorial' n =
   let product = ref 1
   and counter = ref 1 in
   let rec iter () =
      if !counter > n
         then !product
         else
            begin
               product := !product * !counter;
               counter := !counter + 1;
               iter ()
            end
   in iter ()

(* Exercise 3.7 * )
( * exercise left to reader to define appropriate functions
   let paul_acc = make_joint peter_acc "open_sesame" "rosebud" *)

(* 3.2.1 - The Environment Model of Evaluation - The Rules for Evaluation *)
let square x = x * x

let square' = fun x -> x * x

(* 3.2.2 - The Environment Model of Evaluation - Applying Simple Procedures *)
let square'' x = x * x

let sum_of_squares x y =
   square x + square y

let f a =
   sum_of_squares (a + 1) (a * 2)

(* exercise 3.9 *)
let rec factorial_2 = function
   | 0 -> 1
   | n -> n * factorial_2 (n-1)

let rec fact_iter product counter max_count =
   if counter > max_count
      then product
      else fact_iter (counter * product) (counter + 1) max_count
let factorial_3 n = fact_iter 1 1 n

module sicp_localstate_translation =
(* 3.2.3 - The Environment Model of Evaluation - Frames as Repository of Local State *)
   let make_withdraw balance =
      let balance = ref balance
      in fun amount ->
         if !balance >= amount
            then (balance := !balance - amount; !balance)
            else raise (InsufficientFunds (!balance))

   let w1 = make_withdraw 100
   let _ = w1 50
   let w2 = make_withdraw 100

   (* Exercise 3.10 *)
   let make_withdraw' initial_amount =
      let balance = ref initial_amount
      in fun amount ->
         if !balance >= amount
            then (balance := !balance - amount; !balance)
            else raise (InsufficientFunds (!balance))

   let w1' = make_withdraw' 100
   let _ = w1' 50
   let w2' = make_withdraw' 100

module sicp_internaldef_translation =
(* 3.2.4 - The Environment Model of Evaluation - Internal Definitions *)
   (* redefine square to work on floats *)
   let square_real' x = x * x

   (* same as in section 1.1.8 *)
   let sqrt' x =
      let good_enough guess =
         abs_float(square_real guess - x) < 0.001
      and improve guess =
         average guess (x / guess) in
      let rec sqrt_iter guess =
         if good_enough guess
            then guess
            else sqrt_iter (improve guess)
      in sqrt_iter 1.0

   let make_account balance =
      let balance = ref balance in
      let withdraw amount =
         if !balance >= amount
            then (balance := !balance - amount; !balance)
            else failwith "Insufficient funds"
      and deposit amount = balance := !balance + amount; !balance
      and getbalance () = !balance
      in { withdraw=withdraw; deposit=deposit; balance=getbalance }

   let acc = make_account 50
   let _ = acc.deposit 40
   let _ = acc.withdraw 60
   let acc2 = make_account 100

(* 3.3.1 - Modeling with Mutable Data - Mutable List Structure *)

(* Note: ML lists can handle types of 'a ref, but this
         can't be used to set the tail of the list.  To
         do this trick, we need to define a list that
         has a ref for the head and tail.  Means extra work for ML.
         A better solution for F# is to use purely functional
         data structures. *)
exception NotFound

type 'a mlist = MNil | MCons of 'a ref * 'a mlist ref
type 'a MLIST = { cons      : 'a -> 'a mlist -> 'a mlist;
                  car       : 'a mlist -> 'a;
                  cdr       : 'a mlist -> 'a mlist;
                  set_car   : 'a mlist -> 'a -> unit;
                  set_cdr   : 'a mlist -> 'a mlist -> unit;
                  make_list : 'a list -> 'a mlist;
                  append    : 'a mlist -> 'a mlist -> 'a mlist }

let MList =
   let cons x y = MCons(ref x, ref y)

   let car = function
      | MCons(x, xs) -> !x
      | MNil -> raise NotFound

   let cdr = function
      | MCons(x, xs) -> !xs
      | MNil -> raise NotFound

   let set_car mlist y =
      match mlist with
       | MCons(x, xs) -> (x := y)
       | MNil -> raise NotFound

   let set_cdr mlist ys =
      match mlist with
       | MCons(x, xs) -> (xs := ys)
       | MNil -> raise NotFound

   let make_list xs = List.fold_right (fun v b -> cons v b) xs MNil

   let rec append mlist ys =
      match mlist with
       | MNil -> ys
       | MCons(x, xs) -> cons (!x) (append (!xs) ys)
   in 
      { cons      = cons;
        car       = car;
        cdr       = cdr;
        set_car   = set_car;
        set_cdr   = set_cdr;
        make_list = make_list;
        append    = append }
   

(* Sharing and identity *)
let x = MList.make_list ["a"; "b"]
let z1 = MList.make_list [x; x]
let z2 = MList.make_list [MList.make_list ["A"; "B"]; MList.make_list ["A"; "B"]]

let set_to_wow x =
   let _ = MList.set_car (MList.car x) "Wow"
   in x

let _ = z1
let _ = set_to_wow z1
let _ = z2
let _ = set_to_wow z2

(* Mutation as assignment *)
type dispatch = Car | Cdr
type ('a,'b) pair' = Left of 'a | Right of 'b
type ('a,'b) PAIR = { cons : 'a -> 'b -> dispatch -> ('a,'b) pair';
                      car  : (dispatch -> ('a,'b) pair') -> 'a;
                      cdr  : (dispatch -> ('a,'b) pair') -> 'b }

module Pair' =
   let cons x y =
      let pdispatch = function
         | Car -> Left x
         | Cdr -> Right y
      in pdispatch

   let car z =
      match z Car with
       | Left c -> c
       | _ -> raise NotFound
   let cdr z =
      match z Cdr with
       | Right c -> c
       | _ -> raise NotFound
   in
      { cons = cons;
        car  = car;
        cdr  = car }

type mdispatch = Car | Cdr | SetCar | SetCdr
type ('a,'b) mpair = Left of 'a
                   | Right of 'b
                   | LSet of ('a -> unit)
                   | RSet of ('b -> unit)

type ('a,'b) MPAIR = { cons    : 'a -> 'b -> mdispatch -> ('a, 'b) mpair;
                       car     : (mdispatch -> ('a, 'b) mpair) -> 'a;
                       cdr     : (mdispatch -> ('a, 'b) mpair) -> 'b;
                       set_car : (mdispatch -> ('a, 'b) mpair) -> 'a -> unit;
                       set_cdr : (mdispatch -> ('a, 'b) mpair) -> 'b -> unit }

let MPair =
   let cons x y =
      let a = ref x
      and b = ref y in
      let setx v = (a := v)
      and sety v = (b := v) in
      let pdispatch = function
         | Car -> Left (!a)
         | Cdr -> Right (!b)
         | SetCar -> LSet setx
         | SetCdr -> RSet sety
      in pdispatch

   let car z =
      match z Car with
       | Left c -> c
       | _ -> raise NotFound
   let cdr z =
      match z Cdr with
       | Right c -> c
       | _ -> raise NotFound

   let set_car z x =
      match z SetCar with
       | LSet f -> f x
       | _ -> raise NotFound

   let set_cdr z y =
      match z SetCar with
       | RSet f -> f y
       | _ -> raise NotFound
   in
      { cons    = cons;
        car     = car;
        cdr     = cdr;
        set_car = set_car;
        set_cdr = set_cdr }

(* This example does not require dynamic dispatch and run-time errors.
   Indeed, there is nothing dynamic about this example. It was implemented
   that way in Scheme because this is all that Scheme can do.  In F#,
   you can use static checking to eliminate all run-time errors: *)
type ('a, 'b) cell =
   { car: unit -> 'a;
     cdr: unit -> 'b;
     set_car: 'a -> unit;
     set_cdr: 'b -> unit }

let cons x y =
   let x = ref x
   and y = ref y
   in
      { car = (fun () -> !x);
        cdr = (fun () -> !y);
        set_car = (fun x' -> x := x');
        set_cdr = (fun y' -> y := y') }

let xa = cons 1 2
let za = cons xa xa
(za.cdr()).set_car 17
xa.car() 

(* Exercise 3.12 *)
let rec last_pair xs =
   match MList.cdr xs with
    | MNil -> xs
    | tail -> last_pair tail

let rec mappend xs ys =
   let _ = MList.set_cdr (last_pair xs) ys
   in xs

let xb = MList.make_list ['a'; 'b']
let yb = MList.make_list ['c'; 'd']
let zb = mappend xb yb
let _ = zb
let wb = mappend xb yb
let _ = wb
let _ = xb

(* Exercise 3.13 *)
let make_cycle xs =
   let _ = MList.set_cdr (last_pair xs) xs
   in xs
let zc = make_cycle (MList.make_list ['a'; 'b'; 'c'])

(* Exercise 3.14 *)
let mystery x =
   let rec loop x y =
      match x with
       | MNil -> y
       | _ ->
         let temp = MList.cdr x
         in ( MList.set_cdr x y; loop temp x )
   in loop x MNil
let v = MList.make_list ['a'; 'b'; 'c'; 'd']
let wd = mystery v

(* Exercise 3.16 *)
(* Write a function to count the number of cons cells in a binary tree. In
   F#, a cons cell is a Node: *)
type t = CEmpty | CNode of t * t

(* The following incorrect code from the book fails to take cyclic trees into account: *)
let rec count = function
   | CNode(a, b) -> 1 + count a + count b
   | CEmpty -> 0

(* Exercise 3.20 *)
let xe = MPair.cons 1 2
let ze = MPair.cons xe xe
MPair.set_car (MPair.cdr ze) 17
MPair.car xe
