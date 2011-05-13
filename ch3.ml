(* SICP Chapter #03 Examples in Alice ML / Standard ML *)
(* Functions defined in previous chapters *)
fun gcd (a, 0) = a
  | gcd (a, b) = gcd(b, a mod b)
fun square_real x:real = x * x
fun average (x, y) =
   (x + y) / 2.0;
fun has_no_divisors(n, 1) = true
  | has_no_divisors(n, c) =
      if n mod c = 0
         then false
         else has_no_divisors(n, c-1)
fun isPrime(n) = has_no_divisors(n, n-1)
fun enumerate_interval(low, high) =
   if low > high
      then nil
      else low::enumerate_interval(low+1, high);
fun isOdd n = ((n mod 2) = 1)
val isEven = not o isOdd


(* 3.1.1 - Assignment and Local State - Local State Variables *)
val balance = ref 100

exception InsufficientFunds of int

fun withdraw amount =
   if !balance >= amount
      then (balance := !balance - amount; !balance)
      else raise InsufficientFunds (!balance)

val _ = withdraw 25
val _ = withdraw 25
val _ = withdraw 60 handle InsufficientFunds b => b
val _ = withdraw 15;

local
   val balance = ref 100
in
   fun new_withdraw amount =
      if !balance >= amount
         then (balance := !balance - amount; !balance)
         else raise InsufficientFunds (!balance)
end;

fun make_withdraw init_balance =
   let
      val balance = ref init_balance
   in
      fn amount =>
         if !balance >= amount
            then (balance := !balance - amount; !balance)
            else raise InsufficientFunds (!balance)
   end

val w1 = make_withdraw 100
val w2 = make_withdraw 100

val _ = w1 50
val _ = w2 70
val _ = w2 40 handle InsufficientFunds b => b
val _ = w1 40

fun make_account init_balance =
   let
      val balance = ref init_balance
      fun withdraw amount =
         if !balance >= amount
            then (balance := !balance - amount; !balance)
            else raise InsufficientFunds (!balance)
      fun deposit amount =
         (balance := !balance + amount; balance)
      fun getbalance () = !balance
   in
      { withdraw=withdraw, deposit=deposit, balance=getbalance }
   end

val acc = make_account 100
val _ = #withdraw acc 50
val _ = #withdraw acc 60 handle InsufficientFunds b => b
val _ = #deposit acc 40
val _ = #withdraw acc 60

val acc2 = make_account 100

(* Exercise 3.1 *)
(* exercise left to reader to define appropriate functions
   val a = make_accumulator 5
   val _ = #f a 10
   val _ = #f a 10 *)

(* Exercise 3.2 *)
(* exercise left to reader to define appropriate functions
   val s = make_monitored sqrt
   val _ = #f s 100
   val _ = #how-many-calls s () *)

(* Exercise 3.3 *)
(* exercise left to reader to define appropriate functions
   val acc make_account(100, "secret-password")
   val _ = #withdraw acc(40, "secret-password")
   val _ = #withdraw acc(50, "some-other-password") *)

(* 3.1.2 - Assignment and Local State - The Benefits of Introducing Assignment *)
val random_init = ref 7

fun rand_update x =
   let
      val a = 27
      val b = 26
      val m = 127
   in
    ((a * x) + b) mod m
   end

fun rand () =
   let
      val x = random_init
   in
      x := rand_update(!x);
      !x
   end

fun cesaro_test () =
   (gcd(rand(), rand()) = 1)

fun monte_carlo (trials, experiment) =
   let
      fun iter (0, trials_passed) = Real.fromInt trials_passed / Real.fromInt trials
        | iter (trials_remaining, trials_passed) =
            if experiment()
               then iter(trials_remaining - 1, trials_passed + 1)
               else iter(trials_remaining - 1, trials_passed)
   in
      iter(trials, 0)
   end

fun estimate_pi trials =
   Math.sqrt(6.0 / monte_carlo(trials, cesaro_test))

(* second version (no assignment) *)
fun random_gcd_test (trials, initial_x) =
   let
      fun iter (trials_remaining, trials_passed, x) =
         let
            val x1 = rand_update x
            val x2 = rand_update x1
         in
            if trials_remaining = 0
               then Real.fromInt trials_passed / Real.fromInt trials
               else
                  if gcd(x1, x2) = 1
                     then iter(trials_remaining - 1, trials_passed + 1, x2)
                     else iter(trials_remaining - 1, trials_passed, x2)
         end
   in
      iter(trials, 0, initial_x)
   end

fun estimate_pi trials =
   Math.sqrt(6.0 / random_gcd_test(trials, !random_init))

(* Exercise 3.6 *)
(* exercise left to reader to define appropriate functions
   fun random_in_range (low, high) =
      let
         val range = high - low
      in
         low + random range
      end *)

(* 3.1.3 - Assignment and Local State - The Cost of Introducing Assignment *)
fun make_simplified_withdraw init_balance =
   let
      val balance = ref init_balance
   in
      fn amount => (balance := !balance - amount; !balance)
   end

val w = make_simplified_withdraw 25
val _ = w 20
val _ = w 10

fun make_decrementer balance =
   fn amount => balance - amount

val d = make_decrementer 25
val _ = d 20
val _ = d 10

val _ = (make_decrementer 25) 20
val _ = (fn amount => (25 - amount)) 20
val _ = 25 - 20

val _ = (make_simplified_withdraw 25) 20

(* Sameness and change *)
val d1 = make_decrementer 25
val d2 = make_decrementer 25

val w1 = make_simplified_withdraw 25
val w2 = make_simplified_withdraw 25
val _ = w1 20
val _ = w1 20
val _ = w2 20

val peter_acc = make_account 100
val paul_acc = make_account 100

val peter_acc = make_account 100
val paul_acc = peter_acc

(* Pitfalls of imperative programming *)
fun factorial n =
   let
      fun iter (product, counter) =
         if counter > n
            then product
            else iter(counter * product, counter + 1)
   in
      iter(1, 1)
   end

fun factorial n =
   let
      val product = ref 1
      val counter = ref 1
      fun iter () =
         if !counter > n
            then !product
            else
               let in
                  product := !counter * !product;
                  counter := !counter + 1;
                  iter()
               end
   in
      iter()
   end

(* Exercise 3.7 *)
(* exercise left to reader to define appropriate functions
   val paul_acc = make_joint(peter_acc, "open_sesame", "rosebud") *)

(* 3.2.1 - The Environment Model of Evaluation - The Rules for Evaluation *)
fun square x = x * x

val square = fn x => x * x

(* 3.2.2 - The Environment Model of Evaluation - Applying Simple Procedures *)
fun square x = x * x

fun sum_of_squares (x, y) =
   square x + square y

fun f a =
   sum_of_squares(a + 1, a * 2)

(* Exercise 3.9 *)
fun factorial 1 = 1
  | factorial n = n * factorial(n - 1)

fun fact_iter (product, counter, max_count) =
   if counter > max_count
      then product
      else fact_iter(counter * product, counter + 1, max_count)

fun factorial n = fact_iter(1, 1, n)

(* 3.2.3 - The Environment Model of Evaluation - Frames as Repository of Local State *)
fun make_withdraw init_balance =
   let
      val balance = ref init_balance
   in
      fn amount =>
         if !balance >= amount
            then (balance := !balance - amount; !balance)
            else raise InsufficientFunds (!balance)
   end

val w1 = make_withdraw 100
val _ = w1 50
val w2 = make_withdraw 100

(* Exercise 3.10 *)
fun make_withdraw initial_amount =
   let
      val balance = ref initial_amount
   in
      fn amount =>
         if !balance >= amount
            then (balance := !balance - amount; !balance)
            else raise InsufficientFunds (!balance)
   end

val w1 = make_withdraw 100
val _ = w1 50
val w2 = make_withdraw 100

(* 3.2.4 - The Environment Model of Evaluation - Internal Definitions *)

(* same as in section 1.1.8 *)
fun sqrt x =
   let
      fun good_enough guess =
         abs(square_real guess - x) < 0.001

      fun improve guess =
         average(guess, x / guess);

      fun sqrt_iter guess =
         if good_enough guess
            then guess
            else sqrt_iter(improve guess)
   in
      sqrt_iter 1.0
   end

(* Exercise 3.11 *)
fun make_account init_balance =
   let
      val balance = ref init_balance
      fun withdraw amount =
         if !balance >= amount
            then (balance := !balance - amount; !balance)
            else raise InsufficientFunds (!balance)
      fun deposit amount =
         (balance := !balance + amount; !balance)
      fun getbalance () = !balance
   in
      { withdraw=withdraw, deposit=deposit, balance=getbalance }
   end

val acc = make_account 50
val _ = #deposit acc 40
val _ = #withdraw acc 60
val acc2 = make_account 100

(* 3.3.1 - Modeling with Mutable Data - Mutable List Structure *)

(* Note: ML lists can handle types of 'a ref, but this
         can't be used to set the tail of the list.  To
         do this trick, we need to define a list that
         has a ref for the head and tail.  Means extra work for ML.
         A better solution for Alice ML is to use purely functional
         data structures.  See PFDS on Alice website or the CTM
         translation for better exposition on Lists in Alice ML. *)
signature MLIST =
   sig
      datatype 'a mlist = MNil | MCons of 'a ref * 'a mlist ref
      val cons      : 'a * 'a mlist -> 'a mlist
      val car       : 'a mlist -> 'a
      val cdr       : 'a mlist -> 'a mlist
      val set_car   : 'a mlist * 'a -> unit
      val set_cdr   : 'a mlist * 'a mlist -> unit
      val make_list : 'a list -> 'a mlist
      val append    : 'a mlist * 'a mlist -> 'a mlist
   end

structure MList : MLIST =
   struct
      datatype 'a mlist = MNil | MCons of 'a ref * 'a mlist ref

      fun cons (x, y) = MCons(ref x, ref y)

      fun car (MCons(x, xs)) = !x
        | car MNil = raise Domain

      fun cdr (MCons(x, xs)) = !xs
        | cdr MNil = raise Domain

      fun set_car (MCons(x, xs), y) = (x := y)
        | set_car (MNil, _) = raise Domain

      fun set_cdr (MCons(x, xs), ys) = (xs := ys)
        | set_cdr (MNil, _) = raise Domain

      fun make_list xs = foldr (fn (v, b) => cons(v, b)) MNil xs

      fun append (MNil, ys) = ys
        | append (MCons(x, xs), ys) = cons(!x, append(!xs, ys))
   end

(* Sharing and identity *)
val x = MList.make_list ["a", "b"]
val z1 = MList.make_list [x, x]
val z2 = MList.make_list [MList.make_list ["A", "B"], MList.make_list ["A", "B"]]

fun set_to_wow x =
   let in
      MList.set_car(MList.car x, "Wow");
      x
   end;

z1;
set_to_wow z1;
z2;
set_to_wow z2;

(* Mutation as assignment *)
signature PAIR' =
   sig
      datatype dispatch = Car | Cdr
      datatype ('a,'b) pair' = Left of 'a | Right of 'b
      val cons : 'a * 'b -> dispatch -> ('a,'b) pair'
      val car : (dispatch -> ('a,'b) pair') -> 'a
      val cdr : (dispatch -> ('a,'b) pair') -> 'b
   end

structure Pair' : PAIR' =
   struct
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
   end

signature MPAIR =
   sig
      datatype dispatch = Car | Cdr | SetCar | SetCdr
      datatype ('a,'b) mpair = Left of 'a
                             | Right of 'b
                             | LSet of ('a -> unit)
                             | RSet of ('b -> unit)
     val cons    : 'a * 'b -> dispatch -> ('a, 'b) mpair
     val car     : (dispatch -> ('a, 'b) mpair) -> 'a
     val cdr     : (dispatch -> ('a, 'b) mpair) -> 'b
     val set_car : (dispatch -> ('a, 'b) mpair) * 'a -> unit
     val set_cdr : (dispatch -> ('a, 'b) mpair) * 'b -> unit
   end

structure MPair : MPAIR =
   struct
      datatype dispatch = Car | Cdr | SetCar | SetCdr
      datatype ('a,'b) mpair = Left of 'a
                             | Right of 'b
                             | LSet of ('a -> unit)
                             | RSet of ('b -> unit)

      fun cons (x, y) =
         let
            val a = ref x
            val b = ref y
            fun setx v = (a := v)
            fun sety v = (b := v)
            fun dispatch Car = Left (!a)
              | dispatch Cdr = Right (!b)
              | dispatch SetCar = LSet setx
              | dispatch SetCdr = RSet sety
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

      fun set_car (z, x) =
         case z SetCar of
            LSet f => f x
          | _ => raise Domain
      fun set_cdr (z, y) =
         case z SetCar of
            RSet f => f y
          | _ => raise Domain
   end

(* This example does not require dynamic dispatch and run-time errors.
   Indeed, there is nothing dynamic about this example. It was implemented
   that way in Scheme because this is all that Scheme can do.  In ML,
   you can use static checking to eliminate all run-time errors: *)
type ('a, 'b) cell = { car: unit -> 'a,
                       cdr: unit -> 'b,
                       set_car: 'a -> unit,
                       set_cdr: 'b -> unit }

fun cons (x, y) =
   let
      val x = ref x
      val y = ref y
   in
      { car = (fn () => !x),
        cdr = (fn () => !y),
        set_car = (fn x' => x := x'),
        set_cdr = (fn y' => y := y') }
   end

val x = cons(1, 2)
val z = cons(x, x);
(#set_car ((#cdr z)())) 17;
(#car x)();


(* Exercise 3.12 *)
fun last_pair xs =
   case MList.cdr xs of
      MList.MNil => xs
    | tail => last_pair tail

fun mappend' (xs, ys) =
   let
      val _ = MList.set_cdr (last_pair xs, ys);
   in
      xs
   end

val x = MList.make_list ["a", "b"]
val y = MList.make_list ["c", "d"]
val z = MList.append(x, y);
z;
val w = mappend'(x, y);
w;
x;

(* Exercise 3.13 *)
fun make_cycle xs =
   let in
      MList.set_cdr(last_pair xs, xs);
      xs
   end
val z = make_cycle(MList.make_list ["a", "b", "c"])

(* Exercise 3.14 *)
fun mystery x =
   let
      fun loop (MList.MNil, y) = y
        | loop (x, y) =
            let
               val temp = MList.cdr x
            in
               MList.set_cdr(x, y);
               loop(temp, x)
            end
   in
      loop(x, MList.MNil)
   end
val v = MList.make_list ["a", "b", "c", "d"]
val w = mystery v

(* Exercise 3.16 *)
(* To Be Done
datatype 'a lol = Val of 'a | Lst of 'a lol list
fun count_pairs MNil = 0
  | count_pairs (Val x::xs) = count_pairs xs
  | count_pairs (Lst _::xs) =  1 + count_pairs xs
*)

(* Exercise 3.20 *)
val x = MPair.cons(1, 2)
val z = MPair.cons(x, x);
MPair.set_car(MPair.cdr z, 17);
MPair.car x;

(* 3.3.2 - Modeling with Mutable Data - Representing Queues *)
exception Queue

signature QUEUE' =
   sig
      type 'a queue
      val empty  : 'a queue -> bool
      val make   : unit -> 'a queue
      val front  : 'a queue -> 'a
      val insert : 'a queue * 'a -> unit
      val delete : 'a queue -> 'a
   end

structure Queue : QUEUE' =
   struct
      datatype 'a qnode = Null | Node of 'a * 'a qnode ref
      type 'a queue = 'a qnode ref * 'a qnode ref

      fun front_ptr (a, b) = a
      fun rear_ptr (a, b) = b

      fun set_car ((a, b), item) = (a := item)
      fun set_cdr ((a, b), item) = (b := item)

      fun set_front_ptr (q, item) = set_car(q, item)
      fun set_rear_ptr (q, item) = set_cdr(q, item)

      fun empty q =
         case !(front_ptr q) of
            Null => true
          | _ => false

      fun make () =
         let
            val n = Null
         in
            (ref n, ref n)
         end

      fun front q =
         case !(front_ptr q) of
            Node(a, _) => a
          | _ => raise Queue

      fun insert (q, item) =
         let
            val n = Node (item, ref Null)
         in
            case !(front_ptr q) of
               Null => ( set_front_ptr(q, n); set_rear_ptr(q, n) )
             | _ =>
                case !(rear_ptr q) of
                   Node(_, nxt) => ( nxt := n; set_rear_ptr(q, n) )
                 | _ => raise Queue
         end

      fun delete q =
         case !(front_ptr q) of
            Node(_, nxt) =>
             let
                val item = front(q)
             in
                set_front_ptr(q, !nxt);
                item
             end
          | _ => raise Queue;
   end

(* Exercise 3.21 *)
(* The following generates error in SML-NJ (value restriction) *)
val q1 = Queue.make() : string Queue.queue;
Queue.insert(q1, "a");
Queue.insert(q1, "b");
Queue.delete q1;
Queue.delete q1;

(* 3.3.3 - Modeling with Mutable Data - Representing Tables *)

datatype ('a, 'b) dictionary = Tab of ('a, 'b) dictionary ref
                             | Tree of 'a * 'b * ('a, 'b) dictionary ref
                             | Leaf

fun assoc (key, Tab xs) = assoc(key, !xs)
  | assoc (key, Leaf) = Leaf
  | assoc (key, record as (Tree(k, v, xs))) =
      if key = k
         then record
         else assoc(key, !xs)


fun lookup (key, table) =
   let
      val record = assoc(key, table)
   in
      case record of
         Tree(k, v, _) => SOME (!v)
       | _ => NONE
   end

fun insert (key, value, table) =
   let
      val record = assoc(key, table)
   in
      case record of
         Tree(k, v, _) => (v := value)
       | _ =>
          case table of
             Tab xs => (xs := Tree(key, ref value, ref (!xs)))
           | _ => raise Domain
   end

fun make_table () = Tab(ref Leaf)

val d = make_table()
val _ = insert("abc", 123, d)
val x = lookup("abc", d)

(* two-dimensional *)
fun lookup2 (key1, key2, table) =
   let
      val record = assoc(key1, table)
   in
      case record of
         Tree(k1, v, _) => lookup(key2, !v)
       | _ => NONE
   end

fun insert2 (key1, key2, value, table) =
   let
      val record = assoc(key1, table)
   in
      case record of
         Tree(k, v, _) => insert(key2, value, !v)
       | _ =>
          case table of
             Tab xs =>
               let
                  val newtab = make_table()
               in
                  insert(key2, value, newtab);
                  xs := Tree(key1, ref newtab, ref (!xs))
               end
           | _ => raise Domain
   end

val d = make_table()
val _ = insert2("abc", 123, 12.3, d)
val x = lookup2("abc", 123, d)

(* local tables *)
signature DICTIONARY2_TYPES =
   sig
      eqtype key1type
      eqtype key2type
      type valtype
   end

signature DICTIONARY2 =
   sig
      type key1type
      type key2type
      type valtype
      val get : key1type * key2type -> valtype option
      val put : key1type * key2type * valtype -> unit
   end

functor Dictionary2 (Types : DICTIONARY2_TYPES) :> DICTIONARY2 where type key1type = Types.key1type
                                                               where type key2type = Types.key2type
                                                               where type valtype = Types.valtype =
   struct
      type key1type = Types.key1type
      type key2type = Types.key2type
      type valtype  = Types.valtype

      datatype ('a, 'b) dictionary = Tab of ('a, 'b) dictionary ref
                                   | Tree of 'a * 'b * ('a, 'b) dictionary ref
                                   | Leaf

      fun make_table () = Tab(ref Leaf) : ('a, 'b) dictionary
      val table = make_table() : (key1type, (key2type, valtype ref) dictionary ref) dictionary

      fun assoc (key, Tab xs) = assoc(key, !xs)
        | assoc (key, Leaf) = Leaf
        | assoc (key, record as (Tree(k, v, xs))) =
            if key = k
               then record
               else assoc(key, !xs)
      fun lookup (key, table) =
         let
            val record = assoc(key, table)
         in
            case record of
               Tree(k, v, _) => SOME (!v)
             | _ => NONE
         end
      fun insert (key, value, table) =
         let
            val record = assoc(key, table)
         in
            case record of
               Tree(k, v, _) => (v := value)
             | _ =>
                case table of
                   Tab xs => (xs := Tree(key, ref value, ref (!xs)))
                 | _ => raise Domain
         end

      fun get (key1, key2) =
         let
            val record = assoc(key1, table)
         in
            case record of
               Tree(k1, v, _) => lookup(key2, !v)
             | _ => NONE
         end
      fun put (key1, key2, value) =
         let
            val record = assoc(key1, table)
         in
            case record of
               Tree(k, v, _) => insert(key2, value, !v)
             | _ =>
                case table of
                   Tab xs =>
                     let
                        val newtab = make_table() : (key2type, valtype ref) dictionary
                     in
                        insert(key2, value, newtab);
                        xs := Tree(key1, ref newtab, ref (!xs))
                     end
                 | _ => raise Domain
         end
   end

(* Andreas Rossberg provided the more concise deinition for Alice ML *)
signature DICTIONARY2_TYPES =
   sig
       eqtype key1type
       eqtype key2type
       type valtype
   end

signature DICTIONARY2 =
   sig
      include DICTIONARY2_TYPES
      val get : key1type * key2type -> valtype option
      val put : key1type * key2type * valtype -> unit
   end

functor Dictionary2 (Types : DICTIONARY2_TYPES) : DICTIONARY2 =
   struct
      open Types

      (* type ('a,'b) dictionary = ('a * 'b) list ref *)

      fun assoc (key, xs) =
         Option.map #2 (List.find (fn (k,x) => k = key) (!xs))

      fun assocIns (key, xs, default) =
         case assoc (key, xs) of
            SOME x => x
          | NONE => (xs := (key, default) :: !xs; default)

      val table = ref nil

      fun get (k1, k2) =
         case assoc (k1, table) of
            NONE => NONE
          | SOME table2 => Option.map ! (assoc (k2, table2))

      fun put (k1, k2, x) =
         assocIns (k2, assocIns (k1, table, ref nil), ref x) := x
   end

(* SML-NJ gives error here *)
structure D = Dictionary2(struct eqtype key1type=string eqtype key2type=int type valtype=real end)
val _ = D.put("abc", 123, 12.3)
val x = D.get("abc", 123)

(* Exercise 3.27 *)
fun fib 0 = 0
  | fib 1 = 1
  | fib n = fib(n - 1) + fib(n - 2)

local
   val table = make_table()
in
   fun memoize f =
      fn x =>
         let
            val previously_computed_result = lookup(x, table)
         in
            case previously_computed_result of
               SOME item => item
             | NONE =>
                  let
                     val result = f x
                  in
                     insert(x, result, table);
                     result
                  end
         end
end

fun memo_fib n =
   let
      fun fib 0 = 0
        | fib 1 = 1
        | fib n = memo_fib(n - 1) + memo_fib(n - 2)
   in
      memoize fib n
   end

(* Alternate translation courtesy of O'Caml code *)
fun memoize f =
   let
      val table = make_table()
      fun f' x =
         case lookup(x, table) of
            SOME item => item
          | NONE =>
               let
                  val result = f f' x
               in
                  insert(x, result, table);
                  result
               end
   in
      f'
   end
fun fib fib 0 = 0
  | fib fib 1 = 1
  | fib fib n = fib (n-1) + fib (n-2)
val mem_fib = memoize fib

(* 3.3.4 - Modeling with Mutable Data - A Simulator for Digital Circuits *)
fun for_each nil f = ()
  | for_each (x::xs) f = (f x; for_each xs f);

fun call_each nil = ()
  | call_each (p::ps) = ( p(); call_each ps )

datatype signal = Hi | Lo

datatype wire = Wire of { get_signal : (unit -> signal),
                          set_signal : (signal -> unit),
                          add_action : ((unit->unit)->unit) }

fun get_signal (Wire{get_signal, ...}) = get_signal()
fun set_signal (Wire{set_signal, ...}, new_value) = set_signal new_value
fun add_action (Wire{add_action, ...}, action_procedure) =  add_action action_procedure

fun make_wire () =
   let
      val signal_value = ref Lo
      val action_procedures = ref nil
      fun set_my_signal new_value =
         if !signal_value <> new_value
            then
               let in
                  signal_value := new_value;
                  call_each (!action_procedures)
               end
            else ()
      fun accept_action_procedure proc =
         action_procedures := proc :: !action_procedures
      fun get_signal () = !signal_value
   in
      Wire{ get_signal = get_signal,
            set_signal = set_my_signal,
            add_action = accept_action_procedure }
   end

fun logical_not Lo = Hi
  | logical_not Hi = Lo

fun logical_and (Hi, Hi) = Hi
  | logical_and _ = Lo

fun logical_or (Lo, Lo) = Lo
  | logical_or _ = Hi


datatype timesegment = TimeSegment of int ref * (unit -> unit) Queue.queue
fun make_time_segment (time, queue) = TimeSegment(ref time, queue)
fun segment_time (TimeSegment(time, q)) = time
fun segment_queue (TimeSegment(time, q)) = q

(* agenda is a list of time segments *)
exception Agenda of string
fun make_agenda () = MList.cons(make_time_segment(0, Queue.make()), MList.MNil)
fun current_time agenda = !(segment_time(MList.car agenda))
fun current_time_ref agenda = segment_time(MList.car agenda)
fun set_current_time (agenda, time) = (current_time_ref agenda) := time

fun segments agenda = MList.cdr agenda
fun set_segments (agenda, segments) = MList.set_cdr(agenda, segments)
fun first_segment agenda = MList.car(segments agenda)
fun rest_segments agenda = MList.cdr(segments agenda)

fun empty_agenda agenda = (segments agenda = MList.MNil)

fun first_agenda_item agenda =
   if empty_agenda agenda
      then raise Agenda "Agenda is empty -- FIRST-AGENDA-ITEM"
      else
         let
            val first_seg = first_segment agenda
         in
            set_current_time(agenda, !(segment_time first_seg));
            Queue.front(segment_queue first_seg)
         end

fun remove_first_agenda_item agenda =
   let
      val q = segment_queue(first_segment agenda)
   in
      Queue.delete q;
      if Queue.empty q
         then set_segments(agenda, rest_segments agenda)
         else ()
   end

fun add_to_agenda (time, action, agenda) =
   let
      fun belongs_before MList.MNil = true
        | belongs_before segments = (time < !(segment_time(MList.car segments)))
      fun make_new_time_segment (time, action) =
         let
            val q = Queue.make()
         in
            Queue.insert(q, action);
            make_time_segment(time, q)
         end
      fun add_to_segments segments =
         if !(segment_time(MList.car segments)) = time
            then Queue.insert(segment_queue(MList.car segments), action)
            else
               let
                  val rest = MList.cdr segments
               in
                  if belongs_before rest
                     then MList.set_cdr(segments, MList.cons(make_new_time_segment(time, action), MList.cdr segments))
                     else add_to_segments rest
               end
      val segments = segments agenda
   in
      if belongs_before segments
         then set_segments(agenda, MList.cons(make_new_time_segment(time, action), segments))
         else add_to_segments segments
   end

val the_agenda = make_agenda()
fun after_delay (delay, action) =
   add_to_agenda(delay + (current_time(the_agenda)), action, the_agenda)

val inverter_delay = 2
val and_gate_delay = 3
val or_gate_delay = 5

fun inverter (input, output) =
   let
      val new_value = logical_not(get_signal(input))
      fun invert_input () =
         after_delay(inverter_delay, fn () => set_signal(output, new_value))
   in
      add_action(input, invert_input)
   end

fun and_gate (a1, a2, output) =
   let
      val new_value = logical_and(get_signal(a1), get_signal(a2))
      fun and_action_procedure () =
         after_delay(and_gate_delay, fn () => set_signal(output, new_value))
   in
      add_action(a1, and_action_procedure);
      add_action(a2, and_action_procedure)
   end

fun or_gate (a1, a2, output) =
   let
      val new_value = logical_or(get_signal(a1), get_signal(a2))
      fun or_action_procedure () =
         after_delay(or_gate_delay, fn () => set_signal(output, new_value))
   in
      add_action(a1, or_action_procedure);
      add_action(a2, or_action_procedure)
   end

fun half_adder (a, b, s, c) =
   let
      val d = make_wire()
      val e = make_wire()
   in
      or_gate(a, b, d);
      and_gate(a, b, c);
      inverter(c, e);
      and_gate(d, e, s)
   end

fun or_gate (a1, a2, output) =
   let
      val b = make_wire()
      val c = make_wire()
      val d = make_wire()
   in
      inverter(a1, b);
      inverter(a2, c);
      and_gate(b, c, d);
      inverter(d, output)
   end

val a = make_wire()
val b = make_wire()
val c = make_wire()
val d = make_wire()
val e = make_wire()
val s = make_wire();

or_gate(a, b, d);
and_gate(a, b, c);
inverter(c, e);
and_gate(d, e, s);

fun full_adder (a, b, c_in, sum, c_out) =
   let
      val s = make_wire()
      val c1 = make_wire()
      val c2 = make_wire()
   in
      half_adder(b, c_in, s, c1);
      half_adder(a, s, sum, c2);
      or_gate(c1, c2, c_out)
   end

fun propagate () =
   if empty_agenda the_agenda
      then ()
      else
         let
            val first_item = first_agenda_item the_agenda
         in
            first_item();
            remove_first_agenda_item the_agenda;
            propagate()
         end

fun probe (name, wire) =
   add_action(
      wire,
      fn () =>
         let
            fun signal_to_string Hi = "Hi"
              | signal_to_string Lo = "Lo"
         in
            print name;
            print " ";
            print (Int.toString(current_time the_agenda));
            print "  New-value = ";
            print (signal_to_string(get_signal wire));
            print "\n"
         end)

(* Sample simulation *)
val input_1 = make_wire()
val input_2 = make_wire()
val sum = make_wire()
val carry = make_wire();

probe("sum", sum);
probe("carry", carry);

half_adder(input_1, input_2, sum, carry);
set_signal(input_1, Hi);
propagate();

set_signal(input_2, Hi);
propagate();

(* Exercise 3.31 *)
(*
fun accept_action_procedure proc =
   action_procedures := proc::action_procedures
*)


(* 3.3.5 - Modeling with Mutable Data - Propagation of Constraints *)

(* Note: Alice ML has built in support for Constraint Programming which better
         addresses the type of problem being solved here.  Sticking with a
         fairly literal translation that doesn't use the native CP of Alice.
         See Alice docs or CTM translation for better exposition on the subject.  *)

exception Constraint of string

(* Note: assign comparable value to propagator since ML does not allow function comparison *)
datatype propagator = Propagator of { funid : int,
                                      process_new_value : unit -> unit,
                                      process_forget_value : unit -> unit }

fun inform_funid (Propagator{funid, ...}) = funid;

local
   val c = ref 0
in
   fun genid() = ( c := !c + 1; !c )
end

fun inform_about_value (Propagator{process_new_value, ...}) =
   process_new_value()

fun inform_about_no_value (Propagator{process_forget_value, ...}) =
   process_forget_value()

datatype 'a connector = Connector of { has_value : unit -> bool,
                                       get_value : unit -> 'a,
                                       set_value : 'a * propagator -> unit,
                                       forget_value : propagator -> unit,
                                       connect : propagator -> unit }

fun has_value (Connector{has_value, ...}) =
   has_value()

fun get_value (Connector{get_value, ...}) =
   get_value()

fun set_value (Connector{set_value, ...}, new_value, informant) =
   set_value(new_value, informant)

fun forget_value(Connector{forget_value, ...}, retractor) =
   forget_value(retractor)

fun connect (Connector{connect, ...}, new_constraint) =
   connect(new_constraint)

fun for_each_except (except, exproc, procedure, listx) =
   let
      fun loop nil = ()
        | loop (x::xs) =
            if exproc x = except
               then loop xs
               else
                  let in
                     procedure x;
                     loop xs
                  end
   in
      loop listx
   end

fun propagator_list_contains (x::xs) v = (inform_funid x = inform_funid v) orelse propagator_list_contains xs v
  | propagator_list_contains nil v = false

fun make_connector () =
   let
      val value_list = ref nil
      val informant_list = ref nil
      val constraints = ref nil
      fun has_value () = (!value_list <> nil)
      fun get_value () = hd (!value_list)
      fun informant () = hd (!informant_list)
      fun set_value (newval, setter) =
         if not(has_value())
            then
               let in
                  value_list := [newval];
                  informant_list := [setter];
                  for_each_except(inform_funid setter, inform_funid, inform_about_value, !constraints)
               end
            else
               if get_value() <> newval
                  then raise Constraint "Contradiction"
                  else ()
      fun forget_value (retractor) =
         if not(List.null(!informant_list)) andalso inform_funid retractor = inform_funid (informant())
            then
               let in
                  informant_list := nil;
                  value_list := nil;
                  for_each_except(inform_funid retractor, inform_funid, inform_about_no_value, !constraints)
               end
            else ()
      fun connect (new_constraint) =
         let in
            if not(propagator_list_contains (!constraints) new_constraint)
               then constraints := new_constraint :: !constraints
               else ();
            if has_value()
               then inform_about_value(new_constraint)
               else ()
         end
   in
      Connector { has_value=has_value,
                  get_value=get_value,
                  set_value=set_value,
                  forget_value=forget_value,
                  connect=connect }
   end

(* Not sure how to get the effect of "me" in SML-NJ? *)
fun adder (a1 : real connector, a2 : real connector, sum : real connector) =
   let
      val me = Promise.promise()
      fun process_new_value () =
         if has_value a1 andalso has_value a2
            then set_value(sum, get_value(a1) + get_value(a2), Promise.future(me))
            else
               if has_value a1 andalso has_value sum
                  then set_value(a2, get_value(sum) - get_value(a1), Promise.future(me))
                  else
                     if has_value a2 andalso has_value sum
                        then set_value(a1, get_value(sum) - get_value(a2), Promise.future(me))
                        else ()
      fun process_forget_value () =
         let in
            forget_value(sum, Promise.future(me));
            forget_value(a1, Promise.future(me));
            forget_value(a2, Promise.future(me));
            process_new_value()
         end
   in
      Promise.fulfill(me, Propagator { funid=genid(),
                                       process_new_value=process_new_value,
                                       process_forget_value=process_forget_value });
      connect(a1, Promise.future(me));
      connect(a2, Promise.future(me));
      connect(sum, Promise.future(me));
      Promise.future(me)
   end

fun multiplier(m1 : real connector, m2 : real connector, product : real connector) =
   let
      val me = Promise.promise()
      fun process_new_value () =
         if (has_value m1 andalso get_value m1 = 0.0) orelse
            (has_value m2 andalso get_value m2 = 0.0)
            then set_value(product, 0.0, Promise.future(me))
            else
               if has_value m1 andalso has_value m2
                  then set_value(product, get_value(m1) * get_value(m2), Promise.future(me))
                  else
                     if has_value product andalso has_value m1
                        then set_value(m2, get_value(product) / get_value(m1), Promise.future(me))
                        else
                           if has_value product andalso has_value m2
                              then set_value(m1, get_value(product) / get_value(m2), Promise.future(me))
                              else ()
      fun process_forget_value () =
         let in
            forget_value(product, Promise.future(me));
            forget_value(m1, Promise.future(me));
            forget_value(m2, Promise.future(me));
            process_new_value()
         end
   in
      Promise.fulfill(me, Propagator { funid=genid(),
                                       process_new_value=process_new_value,
                                       process_forget_value=process_forget_value });
      connect(m1, Promise.future(me));
      connect(m2, Promise.future(me));
      connect(product, Promise.future(me));
      Promise.future(me)
   end

fun constant(value : real, connector : real connector) =
   let
      fun process_new_value () =
         raise Constraint "Unknown request -- CONSTANT -- process_new_value"
      fun process_forget_value () =
         raise Constraint "Unknown request -- CONSTANT  -- process_forget_value"
      val me = Propagator { funid=genid(),
                            process_new_value=process_new_value,
                            process_forget_value=process_forget_value }
   in
      connect(connector, me);
      set_value(connector, value, me);
      me
   end

fun probe (name, connector) =
   let
      fun print_probe value =
         let in
            print "Probe: ";
            print name;
            print " = ";
            print (Real.toString(value));
            print "\n"
         end
      fun process_new_value () =
         print_probe(get_value(connector))
      fun process_forget_value () =
         let in
            print "Probe: ";
            print name;
            print " = ";
            print "?";
            print "\n"
         end
      val me = Propagator { funid=genid(),
                            process_new_value=process_new_value,
                            process_forget_value=process_forget_value }
   in
      connect(connector, me);
      me
   end

val user = Propagator { funid=genid(),
                        process_new_value=fn()=>(),
                        process_forget_value=fn()=>() }

fun celsius_fahrenheit_converter (c, f) =
   let
      val u = make_connector()
      val v = make_connector()
      val w = make_connector()
      val x = make_connector()
      val y = make_connector()
   in
      multiplier(c, w, u);
      multiplier(v, x, u);
      adder(v, y, f);
      constant(9.0, w);
      constant(5.0, x);
      constant(32.0, y);
      ()
   end

val c = make_connector()
val f = make_connector()

val _ = celsius_fahrenheit_converter(c, f)

val _ = probe("Celsius temp", c)
val _ = probe("Fahrenheit temp", f)

val _ = set_value(c, 100.0, user)
val _ = forget_value(c, user)
val _ = set_value(f, 32.0, user)


(* Exercise 3.34 *)
fun squarer (a, b) =
  multiplier(a, a, b)

(* Exercise 3.36 *)
val a = make_connector()
val b = make_connector()
val _ = set_value(a, 10, user)

(* Exercise 3.37 *)
(* exercise left to reader to define appropriate functions
   fun celsius_fahrenheit_converter x =
      c_plus(c_times(c_divide(cv 9, cv 5), x), cv 32)
   val c = make_connector()
   val f = celsius_fahrenheit_converter(c)
   fun c_plus (x, y) =
      let
         val z = make_connector()
      in
         adder(x, y, z)
         z
      end *)

(* 3.4.1 - Concurrency: Time Is of the Essence - The Nature of Time in Concurrent Systems *)

val balance = ref 100

exception InsufficientFunds of int

fun withdraw amount =
   if !balance >= amount
      then (balance := !balance - amount; !balance)
      else raise InsufficientFunds (!balance)

(* Exercise 3.38 *)
val _ = balance := !balance + 10
val _ = balance := !balance - 20
val _ = balance := !balance - (!balance div 2)


(* 3.4.2 - Concurrency: Time Is of the Essence - Mechanisms for Controlling Concurrency *)

fun parallel_execute f1 f2 = ( spawn f1(); spawn f2() )

val x = ref 10;
parallel_execute (fn () => x := !x * !x)
                 (fn () => x := !x + 1);

(* Implementing serializers *)
fun make_mutex () = Lock.lock()

fun make_serializer () =
   let
      val mutex = make_mutex()
   in
      fn p => Lock.sync mutex p
   end

val x = ref 10
val s = make_serializer();
parallel_execute (s (fn () => x := !x * !x))
                 (s (fn () => x := !x + 1));

fun make_account init_balance =
   let
      val balance = ref init_balance
      fun withdraw amount =
         if !balance >= amount
            then (balance := !balance - amount; !balance)
            else raise InsufficientFunds (!balance)
      fun deposit amount =
         (balance := !balance + amount; !balance)
      fun getbalance () = !balance
      val lock = Lock.lock()
   in
      { withdraw=Lock.sync lock withdraw,
        deposit=Lock.sync lock deposit,
        balance=getbalance }
   end

(* Exercise 3.39 *)
val x = ref 10
val s = make_serializer();
parallel_execute (fn () => x := (s (fn () => !x * !x))())
                 (s (fn () => (x := !x + 1; !x)));

(* Exercise 3.40 *)

val x = ref 10;
parallel_execute (fn () => x := !x * !x)
                 (fn () => x := !x * !x * !x);

val x = ref 10
val s = make_serializer();
parallel_execute (s (fn () => x := !x * !x))
                 (s (fn () => x := !x * !x * !x));

(* Exercise 3.41 *)
fun make_account init_balance =
   let
      val balance = ref init_balance
      fun withdraw amount =
         if !balance >= amount
            then (balance := !balance - amount; !balance)
            else raise InsufficientFunds (!balance)
      fun deposit amount =
         (balance := !balance + amount; !balance)
      val lock = Lock.lock()
   in
      { withdraw=Lock.sync lock withdraw,
        deposit=Lock.sync lock deposit,
        balance=Lock.sync lock (fn () => !balance) }
   end


(* Exercise 3.42 *)
fun make_account init_balance =
   let
      val balance = ref init_balance
      fun withdraw amount =
         if !balance >= amount
            then (balance := !balance - amount; !balance)
            else raise InsufficientFunds (!balance)
      fun deposit amount =
         (balance := !balance + amount; !balance)
      fun getbalance () = !balance
      val lock = Lock.lock()
      val protected_withdraw = Lock.sync lock withdraw
      val protected_deposit = Lock.sync lock deposit
   in
      { withdraw=protected_withdraw,
        deposit=protected_deposit,
        balance=getbalance }
   end

(* Multiple shared resources *)
type acct = { withdraw:int->int, deposit:int->int, balance:unit->int, serializer:Lock.t }

fun make_account init_balance =
   let
      val balance = ref init_balance
      fun withdraw amount =
         if !balance >= amount
            then (balance := !balance - amount; !balance)
            else raise InsufficientFunds (!balance)
      fun deposit amount =
         (balance := !balance + amount; !balance)
      fun getbalance () = !balance
      val lock = Lock.lock()
   in
      { withdraw=withdraw,
        deposit=deposit,
        balance=getbalance,
        serializer=lock }
   end

fun exchange (account1:acct, account2:acct) =
   let
      val difference = (#balance account1 ()) - (#balance account2 ())
   in
      #withdraw account1 difference;
      #deposit account2 difference;
      difference
   end

fun deposit (account:acct, amount) =
   let
      val s = #serializer account
      val d = #deposit account
   in
      Lock.sync s (d) (amount)
   end

fun serialized_exchange (account1:acct, account2:acct) =
   let
      val serializer1 = #serializer account1
      val serializer2 = #serializer account2
   in
      Lock.sync serializer1 Lock.sync serializer2 exchange(account1, account2)
   end

(* Exercise 3.44 *)

fun transfer (from_account:acct, to_account:acct, amount) =
   let in
      #withdraw from_account amount;
      #deposit to_account amount
   end

(* Exercise 3.45 *)
type acct = { withdraw:int->int, deposit:int->int, balance:unit->int, serializer:Lock.t }

fun make_account init_balance =
   let
      val balance = ref init_balance
      fun withdraw amount =
         if !balance >= amount
            then (balance := !balance - amount; !balance)
            else raise InsufficientFunds (!balance)
      fun deposit amount =
         (balance := !balance + amount; !balance)
      fun getbalance () = !balance
      val lock = Lock.lock()
   in
      { withdraw=Lock.sync lock withdraw,
        deposit=Lock.sync lock deposit,
        balance=getbalance,
        serializer=lock }
   end

fun deposit (account:acct, amount) =
   #deposit account amount


(* 3.5.1 - Streams - Streams Are Delayed Lists *)

fun sum_primes (a, b) =
   let
      fun iter (count, accum) =
         if count > b
            then accum
            else
               if isPrime count
                  then iter(count + 1, count + accum)
                  else iter(count + 1, accum)
   in
      iter (a, 0)
   end


fun sum_primes (a, b) =
  List.foldr op+ 0 (List.filter isPrime (enumerate_interval(a, b)));

(* hd (tl (List.filter isPrime (enumerate_interval(10000, 1000000)))); *)

fun force a = a
val the_empty_stream = nil
fun stream_null xs = (xs = the_empty_stream)
fun cons_stream (x, xs) = x::(lazy xs)
fun stream_car stream = hd stream
fun stream_cdr stream = force (tl stream)

fun stream_ref (s, n) =
   if n = 0
      then hd s
      else stream_ref(tl s, n - 1)
val stream_ref = List.take

fun lazy stream_map proc nil = nil
  | stream_map proc (x::xs) =
      proc x :: stream_map proc xs

fun lazy stream_for_each proc nil = ()
  | stream_for_each proc (x::xs) =
      let in
         proc x;
         stream_for_each proc xs
      end

fun display_line x =
   let in
      print (Int.toString x);
      print "\n"
   end

fun display_stream s =
   stream_for_each display_line s

fun lazy stream_enumerate_interval (low, high) =
   if low > high
      then nil
      else low :: stream_enumerate_interval(low + 1, high)

fun lazy stream_filter pred nil = nil
  | stream_filter pred (x::xs) =
      if pred x
         then x :: stream_filter pred xs
         else stream_filter pred xs;


stream_car(stream_cdr(stream_filter isPrime (stream_enumerate_interval(10000, 1000000))));

fun memo_proc proc =
   let
      val already_run = ref false
      val result = Promise.promise()
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

(* Exercise 3.51 *)
fun show x =
   let in
      print (Int.toString x);
      print "\n"
   end
val x = stream_map show (stream_enumerate_interval(0, 10));
List.take(x, 5);
List.take(x, 7);

(* Exercise 3.52 *)
val sum = ref 0

fun accum x =
   let in
      sum := !sum + x;
      !sum
   end

val seq = stream_map accum (stream_enumerate_interval(1, 20))
val y = stream_filter isEven seq
val z = stream_filter (fn (x) => (x mod 5) = 0) seq;

List.take(y, 7);
display_stream z;


(* 3.5.2 - Streams - Infinite Streams *)
fun lazy integers_starting_from n =
   n :: integers_starting_from(n + 1)

val integers = integers_starting_from 1

fun isDivisible (x, y) = ((x mod y) = 0)

val no_sevens = stream_filter (fn (x) => not(isDivisible(x, 7))) integers;

List.take(no_sevens, 100);

fun lazy fibgen (a, b) =
   a :: fibgen(b, a + b)

val fibs = fibgen(0, 1)

fun lazy sieve stream =
   hd stream ::
      sieve
         (stream_filter
            (fn x => not(isDivisible(x, hd stream)))
            (tl stream))

val primes = sieve(integers_starting_from 2);

List.take(primes, 50);

(* Defining streams implicitly *)
(*val rec ones =  lazy 1 :: ones*)
fun lazy ones_gen () =  1 :: ones_gen()
val ones = ones_gen()

fun lazy add_streams (x::xs, y::ys) = x + y :: add_streams(xs, ys)
  | add_streams (_, _) = raise Domain

fun lazy integers_gen () = 1::add_streams(ones, integers_gen())
val integers = integers_gen()

fun lazy fibs_gen () = 0::1::add_streams(lazy tl (fibs_gen()), fibs_gen())
val fibs = fibs_gen()

fun scale_stream stream factor =
   stream_map (fn (x) => x * factor) stream

fun lazy double_gen () = 1 :: scale_stream (double_gen()) 2
val double = double_gen()

fun primes_gen () = 2 :: stream_filter isPrime (integers_starting_from 3)
val primes = primes_gen();

fun isPrime n =
   let
      fun iter ps =
         if square (hd ps) > n
            then true
            else
               if isDivisible(n, hd ps)
                  then false
                  else iter(tl ps)
   in
      iter primes
   end

(* Exercise 3.53 *)
fun lazy s_gen () = 1 :: add_streams(s_gen(), s_gen())
val s = s_gen()

(* Exercise 3.56 *)
fun lazy merge (nil, s2) = s2
  | merge(s1, nil) = s1
  | merge (s1, s2) =
      let
         val s1car = hd s1
         val s2car = hd s2
      in
         if s1car < s2car
            then s1car :: merge(tl s1, s2)
            else
               if s1car > s2car
                  then s2car :: merge(s1, tl s2)
                  else s1car :: merge(tl s1, tl s2)
      end

(* Exercise 3.58 *)
fun lazy expand (num, den, radix) =
   (num * radix) div den ::
      expand((num * radix) mod den, den, radix)

(* Exercise 3.59 *)
(* exercise left to reader to define appropriate functions
   fun lazy exp_series_gen () = 1 :: integrate_series exp_series_gen()
   val gen = exp_series_gen() *)


(* 3.5.3 - Streams - Exploiting the Stream Paradigm *)
fun sqrt_improve (guess, x) =
  average(guess, x / guess)


fun sqrt_stream x =
   let
      fun lazy guesses_gen () =
         1.0 :: stream_map (fn (guess) => sqrt_improve(guess, x)) (guesses_gen())
   in
      guesses_gen()
   end;

List.take(sqrt_stream 2.0, 5);

fun lazy add_streams_real (x::xs, y::ys) =
   (x:real) + y :: add_streams_real(xs, ys)
  | add_streams_real (_, _) = raise Domain

fun lazy partial_sums a =
   hd a :: add_streams_real(partial_sums a, tl a)
fun scale_stream_real stream factor =
   stream_map (fn (x:real) => x * factor) stream

fun lazy pi_summands (n:int) : real list =
   1.0 / Real.fromInt n :: stream_map (fn x => 0.0 - x) (pi_summands(n + 2))

fun pi_stream_gen () = scale_stream_real (partial_sums (pi_summands 1)) 4.0
val pi_stream = pi_stream_gen();

List.take(pi_stream, 8);

fun lazy euler_transform (s : real list) =
   let
      val s0 = List.nth(s, 0)
      val s1 = List.nth(s, 1)
      val s2 = List.nth(s, 2)
   in
      s2 - square_real(s2 - s1)/(s0 + ~2.0*s1 + s2) :: euler_transform (tl s)
   end;

List.take(euler_transform pi_stream, 8);

fun lazy make_tableau transform s = s :: make_tableau transform (transform s)

fun accelerated_sequence transform s =
   stream_map hd (make_tableau transform s);

List.take (accelerated_sequence euler_transform pi_stream, 8);

(* Exercise 3.63 *)
fun lazy sqrt_stream x =
   1.0 :: stream_map (fn guess => sqrt_improve(guess, x)) (sqrt_stream x)

(* Exercise 3.64 *)
(* exercise left to reader to define appropriate functions
   fun sqrt (x, tolerance) =
      stream_limit(sqrt_stream x, tolerance) *)

(* Infinite streams of pairs *)
fun lazy stream_append (nil, ys) = ys
  | stream_append (x::xs, ys) =
      x :: stream_append(xs, ys)

fun lazy interleave (nil, ys) = ys
  | interleave (x::xs, ys) =
      x :: interleave(ys, xs)

fun lazy pairs (s, t) =
   [hd s, hd t] :: interleave(stream_map (fn x => [hd s, x]) (tl t), pairs (tl s, tl t))

val _ = pairs(integers, integers)

val int_pairs = pairs(integers, integers)

fun lazy sop_gen () = stream_filter (fn pair => isPrime(hd pair + hd(tl pair))) int_pairs;

(* Exercise 3.68 *)
fun lazy pairs (s, t) =
   interleave(
      stream_map (fn x => [hd s, x]) t,
      pairs (tl s, tl t))

(* Streams as signals *)
fun integral (integrand, initial_value, dt) =
   let
      fun lazy int_gen () = initial_value :: add_streams_real(scale_stream_real integrand dt, int_gen())
   in
      int_gen()
   end

(* Exercise 3.74 *)
(* exercise left to reader to define appropriate functions
   fun lazy make_zero_crossings (input_stream, last_value) =
      sign_change_detector(hd input_stream, last_value) ::
         make_zero_crossings(tl input_stream, tl input_stream)
   val zero_crossings = make_zero_crossings(sense_data, 0) *)

(* Exercise 3.75 *)
(* exercise left to reader to define appropriate functions
   fun lazy make_zero_crossings (input_stream, last_value) =
      let
         val avpt = (hd input_stream + last_value) / 2.0
      in
         sign_change_detector(avpt, last_value) ::
            make_zero_crossings(tl input_stream, avpt)
      end *)


(* 3.5.4 - Streams - Streams and Delayed Evaluation *)
fun solve (f, y0, dt) =
   let
      val dy = Promise.promise()
      val y = integral(Promise.future dy, y0, dt)
   in
      Promise.fulfill(dy, stream_map f y);
      y
   end

fun integral (delayed_integrand, initial_value, dt) =
   let
      val integrand = force delayed_integrand
      fun lazy int_gen () = initial_value :: add_streams_real(scale_stream_real integrand dt, int_gen())
   in
      int_gen()
   end

fun solve (f, y0, dt) =
   let
      val dy = Promise.promise()
      val y = integral(lazy (Promise.future dy), y0, dt)
   in
      Promise.fulfill(dy, stream_map f y);
      y
   end;

List.nth(solve(fn y => y, 1.0, 0.001), 1000);

(* Exercise 3.77 *)
fun lazy integral (integrand, initial_value:real, dt) =
   initial_value ::
      (if integrand = nil
         then nil
         else integral(tl integrand, (dt * hd integrand) + initial_value, dt))


(* 3.5.5 - Streams - Modularity of Functional Programs and Modularity of Objects *)
(* same as in section 3.1.2 *)
fun rand () =
   let
      val x = random_init
   in
      x := rand_update(!x);
      !x
   end

fun lazy random_numbers_gen () =
   !random_init :: stream_map rand_update (random_numbers_gen())
val random_numbers = random_numbers_gen()

fun lazy map_successive_pairs f s =
   f(hd s, hd(tl s)) :: map_successive_pairs f (tl(tl s))

val cesaro_stream =
   map_successive_pairs (fn (r1, r2) => (gcd(r1, r2) = 1)) random_numbers

fun lazy monte_carlo (experiment_stream, passed, failed) =
   let
      fun next (passed, failed) =
         Real.fromInt(passed) / (Real.fromInt (passed + failed)) ::
            monte_carlo(tl experiment_stream, passed, failed)
   in
      if hd experiment_stream
         then next(passed + 1, failed)
         else next(passed, failed + 1)
   end

val pi' = stream_map (fn p => (sqrt(6.0 / p))) (monte_carlo(cesaro_stream, 0, 0))

(* same as in section 3.1.3 *)
fun make_simplified_withdraw balance =
   fn amount => (balance := !balance - amount; !balance)

fun lazy stream_withdraw (balance, amount_stream) =
   balance :: stream_withdraw(balance - hd amount_stream, tl amount_stream)
