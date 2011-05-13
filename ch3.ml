(* SICP Chapter #03 Examples in O'Caml *)
(* Functions defined in previous chapters *)
let rec gcd a = function
   | 0 -> a
   | b -> gcd b (a mod b)
let square_real x = x *. x
let average x y = (x +. y) /. 2.0
let rec has_no_divisors n c =
   c = 1 || (n mod c <> 0 && has_no_divisors n (c-1))
let isPrime n = has_no_divisors n (n-1)
let rec enumerate_interval low high =
   if low > high
      then []
      else low :: enumerate_interval (low+1) high
let compose f g x = f(g x)
let isOdd n = (n mod 2 = 1)
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

(* Record Selector Translation *)
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
(* end Record Selector Translation *)

(* Polymorphic Variants Translation *)
   type vlist = [`Withdraw | `Deposit]
   let make_account balance =
      let balance = ref balance in
      let withdraw amount =
         if !balance >= amount
            then begin balance := (!balance - amount); !balance end
            else raise (InsufficientFunds (!balance))
      and deposit amount =
         balance := (!balance + amount); !balance
      in
         function
            | (`Withdraw : vlist) -> withdraw
            | `Deposit -> deposit

   let acc = make_account 100
   let _ = acc `Withdraw 50
   let _ = try acc `Withdraw 60  with InsufficientFunds b -> b
   let _ = acc `Deposit 40
   let _ = acc `Withdraw 60

   let acc2 = make_account 100
(* end Polymorphic Variants Translation *)

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
   in (a * x + b) mod m

let rand =
   let x = random_init
   in fun () -> x := (rand_update !x); !x

let cesaro_test () = gcd (rand()) (rand()) = 1

let monte_carlo trials experiment =
   let rec iter trials_remaining trials_passed =
      match trials_remaining with
       | 0 -> float_of_int trials_passed /. float_of_int trials
       | _ ->
         if experiment()
            then iter (trials_remaining - 1) (trials_passed + 1)
            else iter (trials_remaining - 1) trials_passed
   in iter trials 0

let estimate_pi trials = sqrt (6. /. (monte_carlo trials cesaro_test))

(* second version (no assignment) *)
let random_gcd_test trials initial_x =
   let rec iter x trials_passed =
      let x1 = rand_update x in
      let x2 = rand_update x1
      in function
         | 0 -> (float trials_passed) /. (float trials)
         | trials_remaining ->
            iter  x2
               (if gcd x1 x2 = 1 then (succ trials_passed) else trials_passed)
               (pred trials_remaining)
   in iter initial_x 0 trials

(* alternate translation *)
let random_gcd_test trials initial_x =
   let rec iter trials_remaining trials_passed x =
      let x1 = rand_update x in
      let x2 = rand_update x1
      in
         if trials_remaining = 0
            then float_of_int trials_passed /. float_of_int trials
            else
               if gcd x1 x2 = 1
                  then iter (trials_remaining - 1) (trials_passed + 1) x2
                  else iter (trials_remaining - 1) trials_passed x2
   in iter trials 0 initial_x

let estimate_pi trials = sqrt (6. /. (random_gcd_test trials !random_init))

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
let d2 = make_decrimenter 25

let w1 = make_simplified_withdraw 25
let w2 = make_simplified_withdraw 25
let _ = w1 20
let _ = w1 20
let _ = w2 20

let peter_acc = make_account 100
let paul_acc = make_account 100

let peter_acc = make_account 100
let paul_acc = peter_acc

(* Pitfalls of imperative programming *)
let factorial n =
   let rec iter product counter =
      if counter > n
         then product
         else iter (counter * product) (counter + 1)
   in iter 1 1

let factorial n =
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

(* Exercise 3.7 *)
(* exercise left to reader to define appropriate functions
   let paul_acc = make_joint peter_acc "open_sesame" "rosebud" *)

(* 3.2.1 - The Environment Model of Evaluation - The Rules for Evaluation *)
let square x = x * x

let square = fun x -> x * x

(* 3.2.2 - The Environment Model of Evaluation - Applying Simple Procedures *)
let square x = x * x

let sum_of_squares x y =
   square x + square y

let f a =
   sum_of_squares (a + 1) (a * 2)

(* exercise 3.9 *)
let rec factorial = function
   | 0 -> 1
   | n -> n * factorial (n-1)

let rec fact_iter product counter max_count =
   if counter > max_count
      then product
      else fact_iter (counter * product) (counter + 1) max_count
let factorial n = fact_iter 1 1 n

(* Alternative definition using optional arguments *)
let rec fact_iter ?(accu=1) = function
  | 0 -> accu
  | n -> fact_iter ~accu:(n*accu) (n-1)
let factorial n = fact_iter ~accu:1 n

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
let make_withdraw initial_amount =
   let balance = ref initial_amount
   in fun amount ->
      if !balance >= amount
         then (balance := !balance - amount; !balance)
         else raise (InsufficientFunds (!balance))

let w1 = make_withdraw 100
let _ = w1 50
let w2 = make_withdraw 100

(* 3.2.4 - The Environment Model of Evaluation - Internal Definitions *)

(* redefine square to work on floats *)
let square_real x = x *. x

(* same as in section 1.1.8 *)
let sqrt' x =
   let good_enough guess =
      abs_float(square_real guess -. x) < 0.001
   and improve guess =
      average guess (x /. guess) in
   let rec sqrt_iter guess =
      if good_enough guess
         then guess
         else sqrt_iter (improve guess)
   in sqrt_iter 1.0

(* Record Selector Translation *)
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
(* end Record Selector Translation *)

(* Polymorphic Variants Translation *)
   let make_account balance =
      let balance = ref balance in
      let withdraw amount =
         if !balance >= amount
            then (balance := !balance - amount; !balance)
            else failwith "Insufficient funds"
      and deposit amount = balance := !balance + amount; !balance
      in
         function
            | (`Withdraw : vlist) -> withdraw
            | `Deposit -> deposit

   let acc = make_account 50
   let _ = acc `Deposit 40
   let _ = acc `Withdraw 60
   let acc2 = make_account 100
(* end Polymorphic Variants Translation *)

(* 3.3.1 - Modeling with Mutable Data - Mutable List Structure *)

(* Note: ML lists can handle types of 'a ref, but this
         can't be used to set the tail of the list.  To
         do this trick, we need to define a list that
         has a ref for the head and tail.  Means extra work for ML.
         A better solution for O'Caml is to use purely functional
         data structures. *)
exception NotFound

module type MLIST =
   sig
      type 'a mlist = MNil | MCons of 'a ref * 'a mlist ref
      val cons      : 'a -> 'a mlist -> 'a mlist
      val car       : 'a mlist -> 'a
      val cdr       : 'a mlist -> 'a mlist
      val set_car   : 'a mlist -> 'a -> unit
      val set_cdr   : 'a mlist -> 'a mlist -> unit
      val make_list : 'a list -> 'a mlist
      val append    : 'a mlist -> 'a mlist -> 'a mlist
   end

module MList : MLIST =
   struct
      type 'a mlist = MNil | MCons of 'a ref * 'a mlist ref

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
   end

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
module type PAIR' =
   sig
      type dispatch = Car | Cdr
      type ('a,'b) pair' = Left of 'a | Right of 'b
      val cons : 'a -> 'b -> dispatch -> ('a,'b) pair'
      val car : (dispatch -> ('a,'b) pair') -> 'a
      val cdr : (dispatch -> ('a,'b) pair') -> 'b
   end

module Pair' : PAIR' =
   struct
      type dispatch = Car | Cdr
      type ('a,'b) pair' = Left of 'a | Right of 'b

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
   end

module type MPAIR =
   sig
      type dispatch = Car | Cdr | SetCar | SetCdr
      type ('a,'b) mpair = Left of 'a
                         | Right of 'b
                         | LSet of ('a -> unit)
                         | RSet of ('b -> unit)
     val cons    : 'a -> 'b -> dispatch -> ('a, 'b) mpair
     val car     : (dispatch -> ('a, 'b) mpair) -> 'a
     val cdr     : (dispatch -> ('a, 'b) mpair) -> 'b
     val set_car : (dispatch -> ('a, 'b) mpair) -> 'a -> unit
     val set_cdr : (dispatch -> ('a, 'b) mpair) -> 'b -> unit
   end

module MPair : MPAIR =
   struct
      type dispatch = Car | Cdr | SetCar | SetCdr
      type ('a,'b) mpair = Left of 'a
                         | Right of 'b
                         | LSet of ('a -> unit)
                         | RSet of ('b -> unit)

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
   end

(* This example does not require dynamic dispatch and run-time errors.
   Indeed, there is nothing dynamic about this example. It was implemented
   that way in Scheme because this is all that Scheme can do.  In OCaml,
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

let x = cons 1 2
let z = cons x x;;
(z.cdr()).set_car 17;;
x.car();;


(* Exercise 3.12 *)
let rec last_pair xs =
   match MList.cdr xs with
    | MList.MNil -> xs
    | tail -> last_pair tail

let rec mappend xs ys =
   let _ = MList.set_cdr (last_pair xs) ys
   in xs

let x = MList.make_list ['a'; 'b']
let y = MList.make_list ['c'; 'd']
let z = mappend x y
let _ = z
let w = mappend x y
let _ = w
let _ = x

(* Exercise 3.13 *)
let make_cycle xs =
   let _ = MList.set_cdr (last_pair xs) xs
   in xs
let z = make_cycle (MList.make_list ['a'; 'b'; 'c'])

(* Exercise 3.14 *)
let mystery x =
   let rec loop x y =
      match x with
       | MList.MNil -> y
       | _ ->
         let temp = MList.cdr x
         in ( MList.set_cdr x y; loop temp x )
   in loop x MList.MNil
let v = MList.make_list ['a'; 'b'; 'c'; 'd']
let w = mystery v

(* Exercise 3.16 *)
(* Write a function to count the number of cons cells in a binary tree. In
   OCaml, a cons cell is a Node: *)
type t = Empty | Node of t * t

(* The following incorrect code from the book fails to take cyclic trees into account: *)
let rec count = function
   | Node(a, b) -> 1 + count a + count b
   | Empty -> 0

(* The following code takes cyclic trees into account: *)
let rec count_c ?(nodes=[]) = function
   | Empty -> 0
   | node when List.memq node nodes -> 0
   | Node(a, b) as node ->
      let nodes = node :: nodes
      in 1 + count_c ~nodes a + count_c ~nodes b

(* Here is an example cyclic tree: *)
let rec cyclic_tree = Node(cyclic_tree, Empty)

(* Exercise 3.20 *)
let x = MPair.cons 1 2
let z = MPair.cons x x;;
MPair.set_car (MPair.cdr z) 17;;
MPair.car x;;

(* 3.3.2 - Modeling with Mutable Data - Representing Queues *)

(* The book is just trying to implement a mutable queue in Scheme.
   The following implements a mutable queue in OCaml: *)
module Queue(Types : sig type t end) =
   struct
      type t = Types.t
      type cell = { mutable prev: cell option;
                    elt: t;
                    mutable next: cell option }
      type typ = { mutable front: cell option;
                   mutable rear: cell option}

      let empty = { front=None; rear=None }

      let is_empty q = q.front=None

      let front q =
         match q.front with
          | None -> raise Not_found
          | Some x -> x.elt

      let insert q x =
         match q.front with
          | None ->
               let cell = Some { prev=None; elt=x; next=None }
               in
                  q.front <- cell;
                  q.rear <- cell
          | Some f ->
               let cell = Some { prev=None; elt=x; next=Some f }
               in
                  f.prev <- cell;
                  q.front <- cell

      let delete q =
         match q.rear with
          | None -> raise Not_found
          | Some r ->
               match r.prev with
                | None ->
                     let x = r.elt
                     in
                        q.front <- None;
                        q.rear <- None;
                        ()
                | Some r' ->
                     let x = r.elt
                     in
                        r'.next <- None;
                        q.rear <- r.prev;
                        ()

      let to_list q =
         let rec loop = function
          | Some c -> c.elt :: loop c.next
          | None -> [] in loop q.front
   end


(* Exercise 3.21 *)
module StringQueue = Queue(struct type t=string end)
let q1 = StringQueue.empty;;
List.iter (StringQueue.insert q1) ["A"; "B"];;
StringQueue.delete q1;;
StringQueue.delete q1;;

(* 3.3.3 - Modeling with Mutable Data - Representing Tables *)

type ('a, 'b) dictionary = Tab of ('a, 'b) dictionary ref
                         | Tree of 'a * 'b * ('a, 'b) dictionary ref
                         | Leaf

let rec assoc key record =
   match record with
    | Tab xs -> assoc key !xs
    | Leaf -> Leaf
    | Tree(k, v, xs) ->
         if key = k
            then record
            else assoc key !xs

let lookup key table =
   let record = assoc key table
   in
      match record with
       | Tree(k, v, _) -> Some (!v)
       | _ -> None

let insert key value table =
   let record = assoc key table
   in
      match record with
       | Tree(k, v, _) -> (v := value)
       | _ ->
          match table with
           | Tab xs -> (xs := Tree(key, ref value, ref (!xs)))
           | _ -> raise NotFound

let make_table () = Tab(ref Leaf)

let d = make_table()
let _ = insert "abc" 123 d
let x = lookup "abc" d

(* two-dimensional *)
let lookup2 key1 key2 table =
   let record = assoc key1 table
   in
      match record with
       | Tree(k1, v, _) -> lookup key2 !v
       | _ -> None

let insert2 key1 key2 value table =
   let record = assoc key1 table
   in
      match record with
       | Tree(k, v, _) -> insert key2 value !v
       | _ ->
          match table with
           | Tab xs ->
               let newtab = make_table()
               in
                  insert key2 value newtab;
                  xs := Tree(key1, ref newtab, ref (!xs))
           | _ -> raise NotFound

let d = make_table()
let _ = insert2 "abc" 123 12.3 d
let x = lookup2 "abc" 123 d

(* local tables *)
module type DICTIONARY2_TYPES =
   sig
      type key1type
      type key2type
      type valtype
   end

module type DICTIONARY2 =
   sig
      include DICTIONARY2_TYPES
      val get : key1type -> key2type -> valtype option
      val put : key1type -> key2type -> valtype -> unit
   end

module Dictionary2 (Types : DICTIONARY2_TYPES) =
   struct
      type key1type = Types.key1type
      type key2type = Types.key2type
      type valtype  = Types.valtype

      type ('a, 'b) dictionary = Tab of ('a, 'b) dictionary ref
                               | Tree of 'a * 'b * ('a, 'b) dictionary ref
                               | Leaf

      let make_table () = Tab(ref Leaf)
      let table = make_table()

      let rec assoc key record =
         match record with
          | Tab xs -> assoc key !xs
          | Leaf -> Leaf
          | Tree(k, v, xs) ->
               if key = k
                  then record
                  else assoc key !xs
      let lookup key table =
         let record = assoc key table
         in
            match record with
             | Tree(k, v, _) -> Some (!v)
             | _ -> None
      let insert key value table =
         let record = assoc key table
         in
            match record with
             | Tree(k, v, _) -> (v := value)
             | _ ->
                match table with
                 | Tab xs -> (xs := Tree(key, ref value, ref (!xs)))
                 | _ -> raise NotFound

      let get key1 key2 =
         let record = assoc key1 table
         in
            match record with
             | Tree(k1, v, _) -> lookup key2 !v
             | _ -> None
      let put key1 key2 value =
         let record = assoc key1 table
         in
            match record with
             | Tree(k, v, _) -> insert key2 value !v
             | _ ->
                match table with
                 | Tab xs ->
                     let newtab = make_table()
                     in
                        begin
                           insert key2 value newtab;
                           xs := Tree(key1, ref newtab, ref (!xs))
                        end
                 | _ -> raise NotFound
   end

module D = Dictionary2(struct type key1type=string type key2type=int type valtype=float end)
let _ = D.put "abc" 123 12.3
let x = D.get "abc" 123

(* Exercise 3.27 *)
let rec fib = function
 | 0 -> 0
 | 1 -> 1
 | n -> fib (n-1) + fib (n-2)

let memoize f =
   let table = make_table()
   in
      fun x ->
         let previously_computed_result = lookup x table
         in
            match previously_computed_result with
             | Some item -> item
             | None ->
                  let result = f x
                  in
                     begin
                        insert x result table;
                        result
                     end

let rec memo_fib n =
   let fib = function
    | 0 -> 0
    | 1 -> 1
    | n -> memo_fib(n - 1) + memo_fib(n - 2)
   in memoize fib n

(* Dynamic dispatch is not imposed upon every function in OCaml, you must add
   it when necessary. To memoize recursive calls inside a function, it is
   necessary to untie the recursive knot, creating a non-recursive function
   that accepts the function that it will recurse into as one of its arguments
*)
let memoize f =
   let m = Hashtbl.create 1 in
   let rec f' x =
      try Hashtbl.find m x with Not_found ->
         let f_x = f f' x
         in
            Hashtbl.add m x f_x;
            f_x
   in f'

let fib fib = function
  | 0 -> 0
  | 1 -> 1
  | n -> fib (n-1) + fib (n-2);;

let mem_fib = memoize fib;;


(* 3.3.4 - Modeling with Mutable Data - A Simulator for Digital Circuits *)
let rec for_each items f =
   match items with
    | [] -> ()
    | x::xs -> (f x; for_each xs f)

let rec call_each = function
 | [] -> ()
 | p::ps -> ( p(); call_each ps )

type signal = Hi | Lo

type wire = { get_signal_rec : unit -> signal;
              set_signal_rec : signal -> unit;
              add_action_rec : (unit->unit)->unit; }

let get_signal pwire = pwire.get_signal_rec()
let set_signal pwire new_value = pwire.set_signal_rec new_value
let add_action pwire action_procedure =  pwire.add_action_rec action_procedure

let make_wire () =
   let signal_value = ref Lo
   and action_procedures = ref [] in
   let set_my_signal new_value =
      if !signal_value <> new_value
         then
            begin
               signal_value := new_value;
               call_each (!action_procedures)
            end
         else ()
   and accept_action_procedure proc =
      action_procedures := proc :: !action_procedures
   and get_signal () = !signal_value
   in
      { get_signal_rec = get_signal;
        set_signal_rec = set_my_signal;
        add_action_rec = accept_action_procedure; }

let logical_not = function
 | Lo -> Hi
 | Hi -> Lo

let logical_and s1 s2 =
   match s1, s2 with
      | Hi, Hi -> Hi
      | _ -> Lo

let logical_or  s1 s2 =
   match s1, s2 with
      | Lo, Lo -> Lo
      | _ -> Hi

module ProcQueue = Queue(struct type t=(unit->unit) end)

type timesegment = TimeSegment of int ref * ProcQueue.typ
let make_time_segment time queue = TimeSegment(ref time, queue)
let segment_time (TimeSegment(time, q)) = time
let segment_queue (TimeSegment(time, q)) = q

(* agenda is a list of time segments *)
exception Agenda of string
let make_agenda () = MList.cons (make_time_segment 0 ProcQueue.empty) MList.MNil
let current_time agenda = !(segment_time(MList.car agenda))
let current_time_ref agenda = segment_time(MList.car agenda)
let set_current_time agenda time = (current_time_ref agenda) := time

let segments agenda = MList.cdr agenda
let set_segments agenda segs = MList.set_cdr agenda segs
let first_segment agenda = MList.car(segments agenda)
let rest_segments agenda = MList.cdr(segments agenda)

let empty_agenda agenda = (segments agenda = MList.MNil)

let first_agenda_item agenda =
   if empty_agenda agenda
      then raise (Agenda "Agenda is empty -- FIRST-AGENDA-ITEM")
      else
         let first_seg = first_segment agenda
         in
            begin
               set_current_time agenda !(segment_time first_seg);
               ProcQueue.front(segment_queue first_seg)
            end

let remove_first_agenda_item agenda =
   let q = segment_queue(first_segment agenda)
   in
      begin
         ProcQueue.delete q;
         if ProcQueue.is_empty q
            then set_segments agenda (rest_segments agenda)
            else ()
      end

let add_to_agenda time action agenda =
   let belongs_before = function
    | MList.MNil -> true
    | segments -> (time < !(segment_time(MList.car segments))) in
   let make_new_time_segment time action =
      let q = ProcQueue.empty
      in
         begin
            ProcQueue.insert q action;
            make_time_segment time q
         end in
   let rec add_to_segments segments =
      if !(segment_time(MList.car segments)) = time
         then ProcQueue.insert (segment_queue(MList.car segments)) action
         else
            let rest = MList.cdr segments
            in
               if belongs_before rest
                  then MList.set_cdr segments (MList.cons (make_new_time_segment time action) (MList.cdr segments))
                  else add_to_segments rest
   and segs = segments agenda
   in
      if belongs_before segs
         then set_segments agenda (MList.cons (make_new_time_segment time action) segs)
         else add_to_segments segs

let the_agenda = make_agenda()
let after_delay delay action =
   add_to_agenda (delay + (current_time the_agenda)) action the_agenda

let inverter_delay = 2
let and_gate_delay = 3
let or_gate_delay = 5

let inverter input output =
   let new_value = logical_not(get_signal input) in
   let invert_input () =
      after_delay inverter_delay  (fun () -> set_signal output new_value)
   in add_action input invert_input

let and_gate a1 a2 output =
   let new_value = logical_and (get_signal a1) (get_signal a2) in
   let and_action_procedure () =
         after_delay and_gate_delay (fun () -> set_signal output new_value)
   in
      begin
         add_action a1 and_action_procedure;
         add_action a2 and_action_procedure
      end

let or_gate a1 a2 output =
   let new_value = logical_or (get_signal a1) (get_signal a2) in
   let or_action_procedure () =
      after_delay or_gate_delay (fun () -> set_signal output new_value)
   in
      begin
         add_action a1 or_action_procedure;
         add_action a2 or_action_procedure
      end

let half_adder a b s c =
   let d = make_wire()
   and e = make_wire()
   in
      begin
         or_gate a b d;
         and_gate a b c;
         inverter c e;
         and_gate d e s
      end

let or_gate a1 a2 output =
   let b = make_wire()
   and c = make_wire()
   and d = make_wire()
   in
      begin
         inverter a1 b;
         inverter a2 c;
         and_gate b c d;
         inverter d output
      end

let a = make_wire()
let b = make_wire()
let c = make_wire()
let d = make_wire()
let e = make_wire()
let s = make_wire();;

or_gate a b d;;
and_gate a b c;;
inverter c e;;
and_gate d e s;;

let full_adder a b c_in sum c_out =
   let s = make_wire()
   and c1 = make_wire()
   and c2 = make_wire()
   in
      begin
         half_adder b c_in s c1;
         half_adder a s sum c2;
         or_gate c1 c2 c_out
      end

let rec propagate () =
   if empty_agenda the_agenda
      then ()
      else
         let first_item = first_agenda_item the_agenda
         in
            begin
               first_item();
               remove_first_agenda_item the_agenda;
               propagate()
            end

let signal_to_string = function
 | Hi -> "Hi"
 | Lo -> "Lo"

let probe name pwire =
   add_action
      pwire
      (fun () ->
         begin
            print_string name;
            print_string " ";
            print_string (string_of_int (current_time the_agenda));
            print_string "  New-value = ";
            print_string (signal_to_string(get_signal pwire));
            print_string "\n"
         end)

(* Sample simulation *)
let input_1 = make_wire()
let input_2 = make_wire()
let sum = make_wire()
let carry = make_wire();;

probe "sum" sum;;
probe "carry" carry;;

half_adder input_1 input_2 sum, carry;;
set_signal input_1 Hi;;
propagate();;

set_signal input_2 Hi;;
propagate();;

(* Exercise 3.31 *)
(*
let accept_action_procedure proc =
   action_procedures := proc::action_procedures
*)


(* 3.3.5 - Modeling with Mutable Data - Propagation of Constraints *)

exception Constraint of string

type propagator = { process_new_value : unit -> unit;
                    process_forget_value : unit -> unit }

let inform_about_value prop = prop.process_new_value()

let inform_about_no_value prop = prop.process_forget_value()

type 'a connector = { has_value : unit -> bool;
                      get_value : unit -> 'a;
                      set_value : 'a -> propagator -> unit;
                      forget_value : propagator -> unit;
                      connect : propagator -> unit; }

let has_value conn = conn.has_value()

let get_value conn = conn.get_value()

let set_value conn new_value informant =
   conn.set_value new_value informant

let forget_value conn retractor =
   conn.forget_value retractor

let connect conn new_constraint =
   conn.connect new_constraint

let for_each_except except procedure listx =
   let rec loop = function
    | [] -> ()
    | x::xs ->
         if x == except
            then loop xs
            else
               begin
                  procedure x;
                  loop xs
               end
   in loop listx

let rec propagator_list_contains items v =
   match items with
    | x::xs -> x == v || propagator_list_contains xs v
    | [] -> false

let is_null = function
 | [] -> true
 | x::xs -> false

let make_connector () =
   let value_list = ref []
   and informant_list = ref []
   and constraints = ref [] in
   let has_value () = (!value_list <> [])
   and get_value () = List.hd (!value_list)
   and informant () = List.hd (!informant_list) in
   let set_value newval setter =
      if not(has_value())
         then
            begin
               value_list := [newval];
               informant_list := [setter];
               for_each_except setter inform_about_value !constraints
            end
         else
            if get_value() <> newval
               then raise (Constraint "Contradiction")
               else ()
   and forget_value retractor =
      if not(is_null !informant_list) && retractor == informant()
         then
            begin
               informant_list := [];
               value_list := [];
               for_each_except retractor inform_about_no_value !constraints
            end
         else ()
   and connect new_constraint =
      begin
         if not(propagator_list_contains (!constraints) new_constraint)
            then constraints := new_constraint :: !constraints
            else ();
         if has_value()
            then inform_about_value new_constraint
            else ()
      end
   in
      { has_value=has_value;
        get_value=get_value;
        set_value=set_value;
        forget_value=forget_value;
        connect=connect }

let adder a1 a2 sum =
   let rec this = { process_new_value=process_new_value;
                    process_forget_value=process_forget_value }
   and process_new_value () =
      if has_value a1 && has_value a2
         then set_value sum (get_value a1 +. get_value a2) this
         else
            if has_value a1 && has_value sum
               then set_value a2 (get_value sum -. get_value a1) this
               else
                  if has_value a2 && has_value sum
                     then set_value a1 (get_value sum -. get_value a2) this
                     else ()
   and process_forget_value () =
      begin
         forget_value sum this;
         forget_value a1 this;
         forget_value a2 this;
         process_new_value()
      end
   in
      connect a1 this;
      connect a2 this;
      connect sum this;
      this

let multiplier m1 m2 product =
   let rec this = { process_new_value=process_new_value;
                    process_forget_value=process_forget_value }
   and process_new_value () =
      if (has_value m1 && get_value m1 = 0.0) ||
         (has_value m2 && get_value m2 = 0.0)
         then set_value product 0.0 this
         else
            if has_value m1 && has_value m2
               then set_value product (get_value m1 *. get_value m2) this
               else
                  if has_value product && has_value m1
                     then set_value m2 (get_value product /. get_value m1) this
                     else
                        if has_value product && has_value m2
                           then set_value m1 (get_value product /. get_value m2) this
                           else ()
   and process_forget_value () =
      begin
         forget_value product this;
         forget_value m1 this;
         forget_value m2 this;
         process_new_value()
      end
   in
      connect m1 this;
      connect m2 this;
      connect product this;
      this

let constant (value : float) (connector : float connector) =
   let rec this = { process_new_value=process_new_value;
                    process_forget_value=process_forget_value }
   and process_new_value () =
      raise (Constraint "Unknown request -- CONSTANT -- process_new_value")
   and process_forget_value () =
      raise (Constraint "Unknown request -- CONSTANT  -- process_forget_value")
   in
      connect connector this;
      set_value connector value this;
      this

let probe name connector =
   let rec this = { process_new_value=process_new_value;
                    process_forget_value=process_forget_value }
   and print_probe value =
      begin
         print_string "Probe: ";
         print_string name;
         print_string " = ";
         print_string (string_of_float value);
         print_string "\n"
      end
   and process_new_value () =
      print_probe(get_value(connector))
   and process_forget_value () =
      begin
         print_string "Probe: ";
         print_string name;
         print_string " = ";
         print_string "?";
         print_string "\n"
      end
in
   connect connector this;
   this


let user = { process_new_value = (fun ()->());
             process_forget_value = (fun ()->()) }

let celsius_fahrenheit_converter c f =
   let u = make_connector()
   and v = make_connector()
   and w = make_connector()
   and x = make_connector()
   and y = make_connector()
   in
      begin
         multiplier c w u;
         multiplier v x u;
         adder v y f;
         constant 9.0 w;
         constant 5.0 x;
         constant 32.0 y;
         ()
      end

let (c : float connector) = make_connector()
let (f : float connector) = make_connector()

let _ = celsius_fahrenheit_converter c f

let _ = probe "Celsius temp" c
let _ = probe "Fahrenheit temp" f

let _ = set_value c 100.0 user
let _ = forget_value c user
let _ = set_value f 32.0 user


(* Exercise 3.34 *)
let squarer a b =
  multiplier a a b

(* Exercise 3.36 *)
let (a : float connector) = make_connector()
let (b : float connector) = make_connector()
let _ = set_value a 10. user

(* Exercise 3.37 *)
(* exercise left to reader to define appropriate functions
   let celsius_fahrenheit_converter x =
      c_plus (c_times (c_divide (cv 9) (cv 5)) x) (cv 32)
   let c = make_connector()
   let f = celsius_fahrenheit_converter(c)
   let c_plus x y =
      let z = make_connector()
      in
         begin
            adder x y z;
            z
         end *)

(* 3.4.1 - Concurrency: Time Is of the Essence - The Nature of Time in Concurrent Systems *)

let balance = ref 100

exception InsufficientFunds of int

let withdraw amount =
   if !balance >= amount
      then (balance := !balance - amount; !balance)
      else raise (InsufficientFunds (!balance))

(* Exercise 3.38 *)
let _ = balance := !balance + 10
let _ = balance := !balance - 20
let _ = balance := !balance - (!balance / 2)


(* 3.4.2 - Concurrency: Time Is of the Essence - Mechanisms for Controlling Concurrency *)

let parallel_execute f1 f2 = ( Thread.create f1(); Thread.create f2() )

let x = ref 10;;
parallel_execute (fun () -> x := !x * !x)
                 (fun () -> x := !x + 1);;

(* Implementing serializers *)
let make_mutex () = Mutex.create()

let make_serializer () =
   let mutex = make_mutex()
   in
      fun p x ->
         begin
            Mutex.lock mutex;
            let v = p x in
               begin
                  Mutex.unlock mutex;
                  v
               end
         end

let x = ref 10
let s = make_serializer();;
parallel_execute (s (fun () -> x := !x * !x))
                 (s (fun () -> x := !x + 1));;

let make_account init_balance =
   let balance = ref init_balance in
   let withdraw amount =
      if !balance >= amount
         then (balance := !balance - amount; !balance)
         else raise (InsufficientFunds (!balance))
   and deposit amount =
      (balance := !balance + amount; !balance)
   and getbalance () = !balance
   in
      { withdraw=make_serializer() withdraw;
        deposit=make_serializer() deposit;
        balance=getbalance }

(* Exercise 3.39 *)
let x = ref 10
let s = make_serializer();;
parallel_execute (fun () -> x := (s (fun () -> !x * !x))())
                 (s (fun () -> (x := !x + 1; !x)));;

(* Exercise 3.40 *)
let x = ref 10;;
parallel_execute (fun () -> x := !x * !x)
                 (fun () -> x := !x * !x * !x);;

let x = ref 10
let s = make_serializer();;
parallel_execute (s (fun () -> x := !x * !x))
                 (s (fun () -> x := !x * !x * !x));;

(* Exercise 3.41 *)
let make_account init_balance =
   let balance = ref init_balance in
   let withdraw amount =
      if !balance >= amount
         then (balance := !balance - amount; !balance)
         else raise (InsufficientFunds (!balance))
   and deposit amount =
      (balance := !balance + amount; !balance)
   in
      { withdraw=make_serializer() withdraw;
        deposit=make_serializer() deposit;
        balance=make_serializer() (fun () -> !balance) }

(* Exercise 3.42 *)
let make_account init_balance =
   let balance = ref init_balance in
   let withdraw amount =
      if !balance >= amount
         then (balance := !balance - amount; !balance)
         else raise (InsufficientFunds (!balance))
   and deposit amount =
      (balance := !balance + amount; !balance)
   and getbalance () = !balance in
   let protected_withdraw = make_serializer() withdraw
   and protected_deposit = make_serializer() deposit
   in
      { withdraw=protected_withdraw;
        deposit=protected_deposit;
        balance=getbalance }

(* Multiple shared resources *)
type ('a, 'b) acct = { withdraw:int->int; deposit:int->int; balance:unit->int; serializer:('a, 'b) Mutex.t }

let make_account init_balance =
   let balance = ref init_balance in
   let withdraw amount =
      if !balance >= amount
         then (balance := !balance - amount; !balance)
         else raise (InsufficientFunds (!balance))
   and deposit amount =
      (balance := !balance + amount; !balance)
   and getbalance () = !balance
   in
      { withdraw=withdraw;
        deposit=deposit;
        balance=getbalance;
        serializer=make_serializer () }

let exchange account1 account2 =
   let difference = account1.balance() - account2.balance()
   in
      account1.withdraw difference;
      account2.deposit difference;
      difference

let deposit account amount =
   let s = account.serializer
   and d = account.deposit
   in s d amount

(* CMR Error: not sure how to get types correct for this example
let serialized_exchange account1 account2 =
   let serializer1 = account1.serializer
   and serializer2 = account2.serializer
   in serializer1 serializer2 exchange account1 account2
*)

(* Exercise 3.44 *)

let transfer from_account to_account amount =
   begin
      from_account.withdraw amount;
      to_account.deposit amount
   end

(* Exercise 3.45 *)
let make_account init_balance =
   let balance = ref init_balance in
   let withdraw amount =
      if !balance >= amount
         then (balance := !balance - amount; !balance)
         else raise (InsufficientFunds (!balance))
   and deposit amount =
      (balance := !balance + amount; !balance)
   and getbalance () = !balance
   and serializer = make_serializer()
   in
      { withdraw=serializer withdraw;
        deposit=serializer deposit;
        balance=getbalance;
        serializer=serializer }

let deposit account amount =
   account.deposit amount


(* 3.5.1 - Streams - Streams Are Delayed Lists *)

let sum_primes a b =
   let rec iter count accum =
      if count > b
         then accum
         else
            if isPrime count
               then iter (count + 1) (count + accum)
               else iter (count + 1) accum
   in iter a 0

let sum_primes a b =
  List.fold_right ( + ) (List.filter isPrime (enumerate_interval a b)) 0;;

(* List.hd (List.tl (List.filter isPrime (enumerate_interval 10000 1000000)));; *)

type 'a node_t =
    | Empty
    | Node of 'a * 'a node_t lazy_t

let force a = Lazy.force a
let the_empty_stream = lazy Empty
let stream_null stream =
   match Lazy.force stream with
    | Empty -> true
    | _ -> false
let cons_stream x xs = lazy (Node(x, xs))

let stream_car stream =
   match Lazy.force stream with
    | Node(x, xs) -> x
    | Empty -> raise Not_found
let stream_cdr stream =
   match Lazy.force stream with
    | Node(x, xs) -> xs
    | Empty -> raise Not_found

let rec take stream n =
   if n == 0
      then []
      else
         match Lazy.force stream with
          | Node(x, xs) -> x :: take xs (n-1)
          | Empty -> raise Not_found
let stream_ref = take

let rec stream_map proc stream = lazy
   begin
      match Lazy.force stream with
       | Empty -> Empty
       | Node(x, xs) -> Node(proc x, stream_map proc xs)
   end

let rec stream_for_each proc stream =
   match Lazy.force stream with
    | Empty -> ()
    | Node(x, xs) ->
         begin
            proc x;
            stream_for_each proc xs
         end

let display_line x =
   begin
      print_string (string_of_int x);
      print_string "\n"
   end

let display_stream stream =
   stream_for_each display_line stream

let rec stream_enumerate_interval low high = lazy
   begin
      if low > high
         then Empty
         else Node(low, stream_enumerate_interval (low + 1) high)
   end

let rec stream_filter pred stream = lazy
   begin
      match Lazy.force stream with
       | Empty -> Empty
       | Node(x, xs) ->
            if (pred x)
               then Node(x, stream_filter pred xs)
               else Lazy.force (stream_filter pred xs)
   end

let s0 = cons_stream 1 (cons_stream 2 (cons_stream 3 (lazy Empty)))
let s1 = stream_enumerate_interval 2 100
let s2 = stream_map isPrime s1
let s3 = stream_filter isPrime s1
let s4 = stream_car(stream_cdr(stream_filter isPrime (stream_enumerate_interval 10000 1000000)))

(* CMR: Still need to convert this one
let memo_proc proc =
   let already_run = ref false
   and result = Promise.promise()
   in
      fn () =>
         if not (!already_run)
            then
               let in
                  already_run := true;
                  Promise.fulfill(result, proc());
                  Promise.future result
               end
            else Promise.future result
   end
*)

(* Exercise 3.51 *)
let show x =
   begin
      print_string (string_of_int x);
      print_string "\n"
   end
let x = stream_map show (stream_enumerate_interval 0 10)
let _ = take x 5
let _ = take x 7

(* Exercise 3.52 *)
let sum = ref 0

let accum x =
   begin
      sum := !sum + x;
      !sum
   end

let seq = stream_map accum (stream_enumerate_interval 1 20)
let y = stream_filter isEven seq
let z = stream_filter (fun x -> (x mod 5) = 0) seq

let _ = take y 7
let _ = display_stream z


(* 3.5.2 - Streams - Infinite Streams *)
let rec integers_starting_from n = lazy
   begin
      Node(n, integers_starting_from (n + 1))
   end

let integers = integers_starting_from 1

let isDivisible x y = ((x mod y) = 0)

let no_sevens = stream_filter (fun x -> not(isDivisible x 7)) integers

let _ = take no_sevens 100

let rec fibgen a b = lazy
   (Node(a, fibgen b (a + b)))

let fibs = fibgen 0 1

let rec sieve stream = lazy
   begin
      match Lazy.force stream with
       | Node(x, xs) -> Node(x, sieve (stream_filter (fun y -> not(isDivisible y x)) xs))
       | Empty -> raise Not_found
   end

let primes = sieve (integers_starting_from 2)

let _ = take primes 50

(* Defining streams implicitly *)
let rec ones_gen () = lazy (Node(1, ones_gen()))
let ones = ones_gen()

let rec add_streams s1 s2 = lazy
   begin
      match Lazy.force s1, Lazy.force s2 with
       | Node(x, xs), Node(y, ys) -> Node(x + y, add_streams xs ys)
       | _, _ -> raise Not_found
   end

let rec integers_gen () = lazy
   begin
      Node(1, add_streams ones (integers_gen()))
   end

let integers = integers_gen()

let rec fibs_gen () = lazy
   begin
      Node(0, lazy ( Node(1, add_streams (stream_cdr (fibs_gen())) (fibs_gen()))))
   end
let fibs = fibs_gen()

let scale_stream stream factor =
   stream_map (fun x -> x * factor) stream

let rec double_gen () = lazy (Node(1, scale_stream (double_gen()) 2))
let double = double_gen()

let primes_gen () = lazy (Node(2, stream_filter isPrime (integers_starting_from 3)))
let primes = primes_gen()

let isPrime n =
   let rec iter ps =
      let x = stream_car ps
      in
         if square x > n
            then true
            else
               if isDivisible n x
                  then false
                  else iter(stream_cdr ps)
   in iter primes

(* Exercise 3.53 *)
let rec s_gen () = lazy (Node(1, add_streams (s_gen()) (s_gen())))
let s = s_gen()

(* Exercise 3.56 *)
let rec merge s1 s2 =
   match Lazy.force s1, Lazy.force s2 with
    | Empty, _ -> s2
    | _, Empty -> s1
    | Node(x, xs), Node(y, ys) ->
         if x < y
            then lazy (Node(x, merge xs s2))
            else
               if x > y
                  then lazy (Node(y, merge s1 ys))
                  else lazy (Node(x, merge xs ys))

(* Exercise 3.58 *)
let rec expand num den radix = lazy
   begin
      Node((num * radix) / den,
         expand ((num * radix) mod den) den radix)
   end

(* Exercise 3.59 *)
(* exercise left to reader to define appropriate functions
   let exp_series_gen () = lazy (Node(1, integrate_series (exp_series_gen())))
   let gen = exp_series_gen()
*)


(* 3.5.3 - Streams - Exploiting the Stream Paradigm *)
let sqrt_improve guess x =
  average guess (x /. guess)

let sqrt_stream x =
   let rec guesses_gen () =
      lazy (Node(1.0, stream_map (fun guess -> sqrt_improve guess x) (guesses_gen())))
   in guesses_gen()

let _ = take (sqrt_stream 2.0) 5

let rec add_streams_real s1 s2 = lazy
   begin
      match Lazy.force s1, Lazy.force s2 with
       | Node(x, xs), Node(y, ys) -> Node(x +. y, add_streams_real xs ys)
       | _, _ -> raise Not_found
   end

let rec partial_sums stream = lazy
   begin
      Node(stream_car stream, add_streams_real (partial_sums stream) (stream_cdr stream))
   end
let scale_stream_real stream factor =
   stream_map (fun x -> x *. factor) stream

let rec pi_summands n = lazy
   begin
      Node(1.0 /. float_of_int n, stream_map (fun x -> 0.0 -. x) (pi_summands (n + 2)))
   end

let pi_stream_gen () = scale_stream_real (partial_sums (pi_summands 1)) 4.0
let pi_stream = pi_stream_gen()

let _ = take pi_stream 8

let rec stream_nth stream n =
   match Lazy.force stream with
    | Node(x, xs) ->
         if n = 0
            then x
            else stream_nth xs (n-1)
    | _ -> raise Not_found

let rec euler_transform stream = lazy
   begin
      let s0 = stream_nth stream 0
      and s1 = stream_nth stream 1
      and s2 = stream_nth stream 2
      in Node(s2 -. square_real(s2 -. s1) /. (s0 +. -2.0 *. s1 +. s2), euler_transform (stream_cdr stream))
   end

let _ = take (euler_transform pi_stream) 8

let rec make_tableau transform stream = lazy (Node(stream, make_tableau transform (transform stream)))

let accelerated_sequence transform stream =
   stream_map stream_car (make_tableau transform stream)

let _ = take (accelerated_sequence euler_transform pi_stream) 8

(* Exercise 3.63 *)
let sqrt_stream x =
   lazy (Node(1.0, stream_map (fun guess -> sqrt_improve guess x) (sqrt_stream x)))

(* Exercise 3.64 *)
(* exercise left to reader to define appropriate functions
   let sqrt x tolerance =
      stream_limit (sqrt_stream x) tolerance *)

(* Infinite streams of pairs *)
let rec stream_append s1 s2 =
   match Lazy.force s1 with
    | Empty -> s2
    | Node(x, xs) -> lazy (Node(x, stream_append xs s2))

let rec interleave s1 s2 =
   match Lazy.force s1 with
    | Empty -> s2
    | Node(x, xs) -> lazy (Node(x, interleave s2 xs))

let rec pairs s t = lazy
   begin
      Node([stream_car s; stream_car t],
         interleave
            (stream_map (fun x -> [stream_car s; x]) (stream_cdr t))
            (pairs (stream_cdr s) (stream_cdr t)))
   end

let _ = pairs integers integers

let int_pairs = pairs integers integers

let sop_gen () =
   stream_filter (fun pair -> isPrime((List.hd pair) + (List.hd (List.tl pair)))) int_pairs

(* Exercise 3.68 *)
(* CMR Error: Laziness not correct for this one - goes into infinite recursion *)
let rec pairs s t =
   begin
      interleave
         (stream_map (fun x -> [stream_car s; x]) t)
         (pairs (stream_cdr s) (stream_cdr t))
   end

(* Streams as signals *)
let integral integrand initial_value dt =
   let rec int_gen () = lazy (Node(initial_value, add_streams_real (scale_stream_real integrand dt) (int_gen())))
   in int_gen()

(* Exercise 3.74 *)
(* exercise left to reader to define appropriate functions
   let rec make_zero_crossings input_stream last_value =
      lazy (Node(sign_change_detector (stream_car input_stream) last_value,
         make_zero_crossings (stream_cdr input_stream) (stream_cdr input_stream)))
   let zero_crossings = make_zero_crossings sense_data 0 *)

(* Exercise 3.75 *)
(* exercise left to reader to define appropriate functions
   let rec make_zero_crossings input_stream last_value =
      let avpt = (stream_car input_stream +. last_value) /. 2.0
      in
         begin
            lazy (Node( sign_change_detector avpt last_value,
               make_zero_crossings (stream_cdr input_stream) avpt))
         end *)
