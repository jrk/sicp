(* SICP Chapter #04 Examples in Alice ML / Standard ML *)
import functor MkHashImpMap     from "x-alice:/lib/data/MkHashImpMap"
import functor MkRedBlackImpMap from "x-alice:/lib/data/MkRedBlackImpMap"

(* 4.1.2 - The Metacircular Evaluator - Representing Expressions *)

(* Note: the parser will need to desugar the function defines of the form:
 *    fun foo (x, y) = (x + y)   into  val foo = (fn (x, y) => x + y)
 *    (define (foo x y) (+ x y)) into  (define foo (lambda (x y) (+ x y)))
 * Note: the parser should also change the cond else to expression (TmBool true)
 *)
exception Evaluator of string

structure Symbol =
   struct
      type t = string
      val compare = String.compare
      fun toString s = s
   end

structure Frame = MkRedBlackImpMap Symbol

datatype term =
     TmUnit
   | TmBool        of bool
   | TmInt         of int
   | TmReal        of real
   | TmString      of string
   | TmQuoted      of term
   | TmIf          of term * term * term
   | TmCond        of (term * term) list
   | TmBegin       of term list
   | TmSymbol      of Symbol.t
   | TmAssignment  of Symbol.t * term
   | TmDefinition  of Symbol.t * term
   | TmLambda      of Symbol.t list * term
   | TmApplication of term * term list

and value =
     ValUnit
   | ValBool       of bool
   | ValInt        of int
   | ValReal       of real
   | ValString     of string
   | ValTuple      of value * value
   | ValQuoted     of term
   | ValSymbol     of Symbol.t
   | ValPrimitive  of Symbol.t * (value list -> value)
   | ValClosure    of Symbol.t list * term * environment
   | ValClosureA   of Symbol.t list * (environment -> value) * environment       (* Closure used for analyze *)

withtype environment = value Frame.map list


(* 4.1.3 - The Metacircular Evaluator - Evaluator Data Structures *)

fun lookup_variable_value (var, frame::enclosing_environment) =
      let
         val valx = Frame.lookup(frame, var)
      in
         case valx of
            NONE   => lookup_variable_value(var, enclosing_environment)
          | SOME v => v
      end
  | lookup_variable_value (var, nil) =
      (print(var ^ "\n"); raise Evaluator("Unbound variable " ^ Symbol.toString(var)) )

fun set_variable_value (var, valx, frame::enclosing_environment) =
      if Frame.member(frame, var)
         then ( Frame.insert(frame, var, valx); valx )
         else set_variable_value(var, valx, enclosing_environment)
  | set_variable_value (var, valx, nil) =
      raise Evaluator("Unbound variable -- SET! " ^ Symbol.toString var)

fun define_variable (var, valx, frame::_) =
      ( Frame.insert(frame, var, valx); valx )
  | define_variable (var, valx, nil) =
      raise Evaluator("Empty Environment " ^ Symbol.toString var)

fun cond2if ((pred, exp)::xs) = TmIf(pred, exp, cond2if xs)
  | cond2if nil               = TmUnit


(* primitive implementations for source language *)
signature LANGUAGE =
   sig
      val make_global_environment : unit -> value Frame.map list
      val valueToString           : value -> string
   end

structure Scheme :> LANGUAGE =
   struct
      fun valueToString x =
         case x of
            ValUnit             => "unit"
          | ValBool v           => Bool.toString v
          | ValInt v            => Int.toString v
          | ValReal v           => Real.toString v
          | ValString v         => "\"" ^ String.toString v ^ "\""
          | ValTuple(x, y)      => "Pair(" ^ valueToString x ^ ", " ^ valueToString y ^ ")"
          | ValQuoted v         => "Quote"
          | ValSymbol v         => v
          | ValPrimitive (v, f) => "<Primitive: " ^ Symbol.toString v ^ ">"
          | ValClosure v        => "<Closure>"
          | ValClosureA v       => "<Closure>"

      fun truth x = case x of ValBool true => true | _ => false

      fun primitive_eq args =
         let
            fun compeq (x, y) =
               case (x, y) of
                  (ValBool a, ValBool b)           => a = b
                | (ValInt a, ValInt b)             => a = b
                | (ValInt a, b)                    => compeq(ValReal(Real.fromInt a), b)
                | (ValReal a, ValReal b)           => Real.==(a, b)
                | (ValString a, ValString b)       => a = b
                | (ValUnit, ValUnit)               => true
                | (ValUnit, ValTuple _)            => false
                | (ValTuple _, ValUnit)            => false
                | (ValTuple(a, b), ValTuple(c, d)) => compeq(a, c) andalso compeq(b, d)
                | _ => raise Evaluator "Invalid Compare"
         in
            case length(args) of
               0 => raise Evaluator "Invalid Number of arguments for compare"
             | 1 => raise Evaluator "Invalid Number of arguments for compare"
             | 2 => ValBool(compeq(hd args, hd(tl args)))
             | _ => ValBool(compeq(hd args, hd(tl args)) andalso
                            truth(primitive_eq(tl args)))
         end

      fun primitive_neq args =
         ValBool(not(truth(primitive_eq args)))

      fun primitive_gt args =
         let
            fun compgt (x, y) =
               case (x, y) of
                  (ValInt a, ValInt b)           => a > b
                | (ValInt a, b)                  => compgt(ValReal(Real.fromInt a), b)
                | (ValReal a, ValReal b)         => a > b
                | _ => raise Evaluator "Invalid Compare"
         in
            case length(args) of
               0 => raise Evaluator "Invalid Number of arguments for compare"
             | 1 => raise Evaluator "Invalid Number of arguments for compare"
             | 2 => ValBool(compgt(hd args, hd(tl args)))
             | _ => ValBool(compgt(hd args, hd(tl args)) andalso
                            truth(primitive_gt(tl args)))
         end

      fun primitive_lt args =
         ValBool(not(truth(primitive_eq args)) andalso not(truth(primitive_gt args)))

      fun primitive_gte args =
         ValBool(truth(primitive_eq args) orelse truth(primitive_gt args))

      fun primitive_lte args =
         ValBool(truth(primitive_eq args) orelse not(truth(primitive_gt args)))

      fun primitive_plus args =
         case args of
            nil => ValInt 0
          | ValInt x :: xs =>
              (case primitive_plus xs of
                  ValInt y => ValInt(x + y)
                | ValReal y => ValReal(Real.fromInt x + y)
                | _ => raise Evaluator("Unexpected error for plus"))
          | ValReal x :: xs =>
              (case primitive_plus xs of
                  ValInt y => ValReal(x + Real.fromInt y)
                | ValReal y => ValReal(x + y)
                | _ => raise Evaluator "Unexpected error for plus")
          | _ => raise Evaluator "Invalid argument for plus"

      fun primitive_minus args =
         case args of
            nil => raise Evaluator "Invalid argument for minus"
          | ValInt x :: xs =>
              (case primitive_plus xs of
                  ValInt y => ValInt(x - y)
                | ValReal y => ValReal(Real.fromInt x - y)
                | _ => raise Evaluator "Unexpected error for minus")
          | ValReal x :: xs =>
              (case primitive_plus xs of
                  ValInt y => ValReal(x - Real.fromInt y)
                | ValReal y => ValReal(x - y)
                | _ => raise Evaluator "Unexpected error for minus")
          | _ => raise Evaluator "Invalid argument for plus"

      fun primitive_multiply args =
         case args of
            nil => ValInt 1
          | ValInt x::nil => ValInt x
          | ValReal x::nil => ValReal x
          | ValInt x :: xs =>
              (case primitive_plus xs of
                  ValInt y => ValInt(x * y)
                | ValReal y => ValReal(Real.fromInt x * y)
                | _ => raise Evaluator "Unexpected error for multiply")
          | ValReal x :: xs =>
              (case primitive_plus xs of
                  ValInt y => ValReal(x * Real.fromInt y)
                | ValReal y => ValReal(x * y)
                | _ => raise Evaluator "Unexpected error for multiply")
          | _ => raise Evaluator "Invalid argument for multiply"

      (* Note: not currently supporting scheme's rational fractions *)
      fun primitive_divide args =
         case args of
            nil => raise Evaluator "Invalid argument for minus"
          | ValInt x :: nil => ValReal(1.0 / Real.fromInt x)
          | ValReal x :: nil => ValReal(1.0 / x)
          | ValInt x :: xs =>
              (case primitive_multiply xs of
                  ValInt 0 => raise Evaluator "Divide by zero error"
                | ValReal 0.0 => raise Evaluator "Divide by zero error"
                | ValInt y => ValReal(Real.fromInt x / Real.fromInt y)
                | ValReal y => ValReal(Real.fromInt x / y)
                | _ => raise Evaluator "Unexpected error for divide")
          | ValReal x :: xs =>
              (case primitive_multiply xs of
                  ValInt y => ValReal(x / Real.fromInt y)
                | ValReal y => ValReal(x / y)
                | _ => raise Evaluator "Unexpected error for divide")
          | _ => raise Evaluator "Invalid argument for divide"

      fun primitive_null [ValUnit] = ValBool true
        | primitive_null (_::_)    = ValBool false
        | primitive_null nil       = ValBool false

      fun primitive_cons (car::cdr::nil) = ValTuple(car, cdr)
        | primitive_cons _ = raise Evaluator "Invalid arguments for cons"

      fun primitive_car [ValTuple(car, cdr)] = car
        | primitive_car _ = raise Evaluator "Invalid arguments for car"

      fun primitive_cdr [ValTuple(car, cdr)] = cdr
        | primitive_cdr _ = raise Evaluator "Invalid arguments for cdr"

      fun primitive_and nil = ValBool true
        | primitive_and (x::nil) = x
        | primitive_and (ValBool false :: _) = ValBool false
        | primitive_and (x::xs) = primitive_and xs

      fun primitive_or nil = ValBool false
        | primitive_or (x::nil) = x
        | primitive_or (ValBool true :: _) = ValBool true
        | primitive_or (ValBool false :: xs) = primitive_or xs
        | primitive_or (x::xs) = x

      fun primitive_not [ValBool false] = ValBool true
        | primitive_not [x] = ValBool false
        | primitive_not _ = raise Evaluator "Invalid number of arguments for not"

      fun primitive_display (x::nil) = ( print(valueToString x ^ "\n"); ValUnit )
        | primitive_display (x::y::nil) = primitive_display(x::nil)
        | primitive_display _ = raise Evaluator "Invalid number of arguments for display"

      fun primitive_string_append args =
         let
            fun iter (s, nil) = ValString s
              | iter (s, ValString x :: xs) = iter(s ^ x, xs)
              | iter _ = raise Evaluator "Invalid arguments for string-append"
         in
            iter("", args)
         end

      fun make_global_environment () =
         let
            val frame = Frame.map()
         in
            Frame.insert(frame, "="            , ValPrimitive("="            , primitive_eq           ));
            Frame.insert(frame, "<>"           , ValPrimitive("<>"           , primitive_neq          ));
            Frame.insert(frame, ">"            , ValPrimitive(">"            , primitive_gt           ));
            Frame.insert(frame, "<"            , ValPrimitive("<"            , primitive_lt           ));
            Frame.insert(frame, ">="           , ValPrimitive(">="           , primitive_gte          ));
            Frame.insert(frame, "<="           , ValPrimitive("<="           , primitive_lte          ));
            Frame.insert(frame, "+"            , ValPrimitive("+"            , primitive_plus         ));
            Frame.insert(frame, "-"            , ValPrimitive("-"            , primitive_minus        ));
            Frame.insert(frame, "*"            , ValPrimitive("*"            , primitive_multiply     ));
            Frame.insert(frame, "/"            , ValPrimitive("/"            , primitive_divide       ));
            Frame.insert(frame, "null?"        , ValPrimitive("null?"        , primitive_null         ));
            Frame.insert(frame, "cons"         , ValPrimitive("cons"         , primitive_cons         ));
            Frame.insert(frame, "car"          , ValPrimitive("car"          , primitive_car          ));
            Frame.insert(frame, "cdr"          , ValPrimitive("cdr"          , primitive_cdr          ));
            Frame.insert(frame, "and"          , ValPrimitive("and"          , primitive_and          ));
            Frame.insert(frame, "or"           , ValPrimitive("or"           , primitive_or           ));
            Frame.insert(frame, "not"          , ValPrimitive("not"          , primitive_not          ));
            Frame.insert(frame, "display"      , ValPrimitive("display"      , primitive_display      ));
            Frame.insert(frame, "string-append", ValPrimitive("string-append", primitive_string_append));
            frame::nil
         end
   end


(* 4.1.1 - The Metacircular Evaluator - The Core of the Evaluator *)

fun eval (TmUnit                , env) = ValUnit
  | eval (TmBool   exp          , env) = ValBool   exp
  | eval (TmInt    exp          , env) = ValInt    exp
  | eval (TmReal   exp          , env) = ValReal   exp
  | eval (TmString exp          , env) = ValString exp
  | eval (TmQuoted exp          , env) = ValQuoted exp
  | eval (TmIf(exp, e1, e2)     , env) = (case eval(exp, env) of ValBool true => eval(e1, env) | _ => eval(e2, env))
  | eval (TmCond exp            , env) = eval(cond2if exp, env)
  | eval (TmBegin exp           , env) = foldl (fn (x, _) => eval(x, env)) ValUnit exp
  | eval (TmSymbol exp          , env) = lookup_variable_value(exp, env)
  | eval (TmDefinition(e1, e2)  , env) = define_variable(e1, eval(e2, env), env)
  | eval (TmAssignment(e1, e2)  , env) = set_variable_value(e1, eval(e2, env), env)
  | eval (TmLambda(parms, body) , env) = ValClosure(parms, body, env)
  | eval (TmApplication(f, args), env) = apply(eval(f, env), map (fn x => eval(x, env)) args)

and apply (ValPrimitive(sym, f), args) = f args
  | apply (ValClosure(parameters, body, env), args) =
      if length parameters <> length args
         then
            if length parameters < length args
               then raise Evaluator "Too many arguments supplied"
               else raise Evaluator "Too few arguments supplied"
         else
            let
               (* create the closure environment *)
               val new_env = Frame.map() :: env
               (* pair up the parameters and arguments into a list *)
               val pairs = ListPair.zip(parameters, args)
            in
               (* push the parameters/arguments into the closure environment *)
               map (fn (x, y) => define_variable(x, y, new_env)) pairs;
               (* evaluate the body of the closure *)
               eval(body, new_env)
            end
  | apply (f, args) = raise Evaluator "Unknown procedure type -- APPLY"


(* 4.1.4 - The Metacircular Evaluator - Running the Evaluator as a Program *)

val the_global_environment = Scheme.make_global_environment()

fun eval_print(code) =
   let
      val valx = eval(code, the_global_environment)
   in
      print (Scheme.valueToString valx);
      print "\n";
      valx
   end;

(* 1 + 6 *)
eval_print(TmApplication(TmSymbol "+", [TmInt 1, TmInt 6]));

(* 1 + (2 * 3) *)
eval_print(TmApplication(TmSymbol "+", [TmInt 1, TmApplication(TmSymbol "*", [TmInt 2, TmInt 3])]));

(* val x = 6 *)
eval_print(TmDefinition("x", TmInt 6));

(* (1 + x) *)
eval_print(TmApplication(TmSymbol "+", [TmInt 1, TmSymbol "x"]));

(* val pi = 3.14 *)
eval_print(TmDefinition("pi", TmReal 3.14));

(* 27.0 / (13.0 - pi) *)
eval_print(TmApplication(TmSymbol "/", [TmReal 27.0, TmApplication(TmSymbol "-", [TmReal 13.0, TmSymbol "pi"])]));

(* val square = fn x => x * x *)
eval_print(TmDefinition("square", TmLambda(["x"], TmApplication(TmSymbol "*", [TmSymbol "x", TmSymbol "x"]))));

(* val z = square(5.0) *)
eval_print(TmDefinition("z", TmApplication(TmSymbol "square", [TmReal 5.0])));

(* fun append (xs, ys) =
      if xs = nil
         then ys
         else hd xs :: append(tl xs, ys) *)
eval_print(
   TmDefinition(
      "append",
      TmLambda(
         ["xs", "ys"],
         TmIf(
            TmApplication(TmSymbol "=", [TmSymbol "xs", TmUnit]),
            TmSymbol "ys",
            TmApplication(
               TmSymbol "cons",
               [
                  TmApplication(TmSymbol "car", [TmSymbol "xs"]),
                  TmApplication(
                     TmSymbol "append",
                     [
                        TmApplication(TmSymbol "cdr", [TmSymbol "xs"]),
                        TmSymbol "ys"
                     ])
               ])))));

(* val xs = ["a", "b", "c"] *)
eval_print(
   TmDefinition(
      "xs",
      TmApplication(
         TmSymbol "cons",
         [
            TmString "a",
            TmApplication(
               TmSymbol "cons",
               [
                  TmString "b",
                  TmApplication(TmSymbol "cons", [TmString "c", TmUnit])
               ])
         ])));

(* val ys = ["d", "e", "f"] *)
eval_print(
   TmDefinition(
      "ys",
      TmApplication(
         TmSymbol "cons",
         [
            TmString "d",
            TmApplication(
               TmSymbol "cons",
               [
                  TmString "e",
                  TmApplication(TmSymbol "cons", [TmString "f", TmUnit])
               ])
         ])));

(* val zs = append(xs, ys) *)
eval_print(TmApplication(TmSymbol "append", [TmSymbol "xs", TmSymbol "ys"]));

(* (cond ((> x 0) x)
         ((= x 0) (display 'zero) 0)
         (else (- x)))  *)
eval_print(
   TmCond(
      [
         (TmApplication(TmSymbol ">", [TmSymbol "x", TmInt 0]), TmSymbol "x"),
         (TmApplication(TmSymbol "=", [TmSymbol "x", TmInt 0]),
            TmBegin([TmApplication(TmSymbol "display", [TmString "zero"]), TmInt 0])),
         (TmBool true, TmApplication(TmSymbol "-", [TmSymbol "x"]))
      ]));

(* if x > 0
      then x
      else
         if x = 0
           then ( print "zero"; 0 )
           else ~x *)
eval_print(
   TmIf(
      TmApplication(TmSymbol ">", [TmSymbol "x", TmInt 0]),
      TmSymbol "x",
      TmIf(
         TmApplication(TmSymbol "=", [TmSymbol "x", TmInt 0]),
         TmBegin([TmApplication(TmSymbol "display", [TmString "zero"]), TmInt 0]),
         TmApplication(TmSymbol "-", [TmSymbol "x"]))));

(* let
      val x = 3
      val y = x + 2
      val z = x + y + 5
   in
      x z
   end *)
eval_print(
   TmApplication(
      TmLambda(
         [],
         TmBegin(
            [
               TmDefinition("x", TmInt 3),
               TmDefinition("y", TmApplication(TmSymbol "+", [TmSymbol "x", TmInt 2])),
               TmDefinition("z", TmApplication(TmSymbol "+",
                  [TmSymbol "x",TmApplication(TmSymbol "+", [TmSymbol "y", TmInt 5])])),
               TmApplication(TmSymbol "*", [TmSymbol "x", TmSymbol "z"])
            ])),
      []));

(* The "and" is not working properly for val.
   The answer given is 5, but it should be 3.
   val x = 1
   let
      val x = 3
      and y = x + 2
   in
      y
   end *)
eval_print(TmDefinition("x", TmInt 1));
eval_print(
   TmApplication(
      TmLambda(
         [],
         TmBegin(
            [
               TmDefinition("x", TmInt 3),
               TmDefinition("y", TmApplication(TmSymbol "+", [TmSymbol "x", TmInt 2])),
               TmSymbol "y"
            ])),
      []));

(* An extension to the eval function should address this problem:
   ((let? exp) (m-eval (let->combination exp) env))
   (define (let->combination let-exp)
     (let ((names (let-bound-variables let-exp))
           (values (let-values let-exp))
           (body (let-body let-exp)))
       (cons (list 'lambda names body) values))) *)

(* fun fib n =
      let
         fun fib_iter (a, b, 0) = b
           | fib_iter (a, b, count) = fib_iter(a + b, a, count - 1)
      in
         fib_iter(1, 0, n)
      end *)
eval_print(
   TmDefinition(
      "fib",
      TmLambda(
         ["n"],
         TmBegin(
            [
               TmDefinition(
                  "fib_iter",
                  TmLambda(
                     ["a", "b", "count"],
                     TmIf(
                        TmApplication(TmSymbol "=", [TmSymbol "count", TmInt 0]),
                        TmSymbol "b",
                        TmApplication(
                           TmSymbol "fib_iter",
                           [
                              TmApplication(TmSymbol "+", [TmSymbol "a", TmSymbol "b"]),
                              TmSymbol "a",
                              TmApplication(TmSymbol "-", [TmSymbol "count", TmInt 1])
                          ])))),
               TmApplication(TmSymbol "fib_iter", [TmInt 1, TmInt 0, TmSymbol "n"])
            ]))));

(* fib 10 *)
eval_print(TmApplication(TmSymbol "fib", [TmInt 10]));


(* 4.1.5 - The Metacircular Evaluator - Data as Programs *)

(* fun factorial n =
      if n = 1
         then 1
         else n * factorial(n - 1) *)
eval_print(
   TmDefinition(
      "factorial",
      TmLambda(
         ["n"],
         TmIf(
            TmApplication(TmSymbol "=", [TmSymbol "n", TmInt 1]),
               TmInt 1,
               TmApplication(
                  TmSymbol "*",
                  [
                     TmSymbol "n",
                     TmApplication(TmSymbol "factorial", [TmApplication(TmSymbol "-", [TmSymbol "n", TmInt 1])])
                  ])))));

(* factorial 5 *)
eval_print(TmApplication(TmSymbol "factorial", [TmInt 5]));


(* (eval '( * 5 5) user-initial-environment) *)
eval_print(TmApplication(TmSymbol "*", [TmInt 5, TmInt 5]));

(* Need to write a parser before I can translate this:
   (eval (cons '* (list 5 5)) user-initial-environment) *)

(* Exercise 4.15 *)
fun run_forever () = run_forever()

fun halts (p, q) = true

exception Halted
fun try p =
   if halts(p, p)
      then run_forever ()
      else raise Halted;

(* 4.1.6 - The Metacircular Evaluator - Internal Definitions *)

(* fun f x =
      let
         fun isEven n =
            if n = 0
               then true
               else isOdd(n-1)
         and isOdd n =
            if n = 0
               then false
               else isEven(n-1)
      in
         ... rest of body of f ...
         isEven x
      end; *)
eval_print(
   TmDefinition(
      "f",
      TmLambda(
         ["x"],
         TmBegin(
            [
               TmDefinition(
                  "isEven",
                  TmLambda(
                     ["n"],
                     TmIf(
                        TmApplication(TmSymbol "=", [TmSymbol "n", TmInt 0]),
                        TmBool true,
                        TmApplication(
                           TmSymbol "isOdd",
                           [TmApplication(TmSymbol "-", [TmSymbol "n", TmInt 1])])))),
               TmDefinition(
                  "isOdd",
                  TmLambda(
                     ["n"],
                     TmIf(
                        TmApplication(TmSymbol "=", [TmSymbol "n", TmInt 0]),
                        TmBool false,
                        TmApplication(
                           TmSymbol "isEven",
                           [TmApplication(TmSymbol "-", [TmSymbol "n", TmInt 1])])))),
               TmApplication(TmSymbol "isEven", [TmSymbol "x"])
            ]))));

eval_print(TmApplication(TmSymbol "f", [TmInt 3]));

(* Exercise 4.19 *)
(* let
      val a = 1
      fun f x =
         let
            val b = a + x
            val a = 5
         in
            a + b
         end
   in
      f 10
   end; *)
eval_print(
   TmBegin(
      [
         TmDefinition("a", TmInt 1),
         TmDefinition(
            "f",
            TmLambda(
               ["x"],
               TmBegin(
                  [
                     TmDefinition("b", TmApplication(TmSymbol "+", [TmSymbol "a", TmSymbol "x"])),
                     TmDefinition("a", TmInt 5),
                     TmApplication(TmSymbol "+", [TmSymbol "a", TmSymbol "b"])
                  ]))),
         TmApplication(TmSymbol "f", [TmInt 10])
      ]));

(* Exercise 4.20 *)
(* fun factorial n =
      if n = 1
         then 1
         else n * factorial(n - 1) *)
eval_print(
   TmDefinition(
      "factorial",
      TmLambda(
         ["n"],
         TmIf(
            TmApplication(TmSymbol "=", [TmSymbol "n", TmInt 1]),
               TmInt 1,
               TmApplication(
                  TmSymbol "*",
                  [
                     TmSymbol "n",
                     TmApplication(TmSymbol "factorial", [TmApplication(TmSymbol "-", [TmSymbol "n", TmInt 1])])
                  ])))));

(* Exercise 4.21 *)
(* Y Combinator in Scheme
   (fn n => (
      (fn fact => fact fact)
      (fn (ft, k) =>
         if k = 1
            then 1
            else k * (ft ft (k-1))))) (10); *)
eval_print(
   TmApplication(
      TmApplication(
         TmLambda(["fact"], TmApplication(TmSymbol "fact", [TmSymbol "fact"])),
         [

            TmLambda(
               ["ft"],
               TmLambda(
                  ["k"],
                  TmIf(
                     TmApplication(TmSymbol "=", [TmSymbol "k", TmInt 1]),
                     TmInt 1,
                     TmApplication(
                        TmSymbol "*",
                        [
                           TmSymbol "k",
                           TmApplication(
                              TmApplication(TmSymbol "ft", [TmSymbol "ft"]),
                              [TmApplication(TmSymbol "-", [TmSymbol "k", TmInt 1])])
                        ]))))
         ]),
      [TmInt 10]));

(* Y Combinator in ML
   fun fix f x = f (fix f) x
   val f = fix (fn ft => fn k => if k = 1 then 1 else k * ft(k-1)) 10; *)
eval_print(
   TmDefinition(
      "fix",
      TmLambda(
         ["f"],
         TmLambda(
            ["x"],
            TmApplication(
               TmApplication(
                  TmSymbol "f",
                  [TmApplication(TmSymbol "fix", [TmSymbol "f"])]),
               [TmSymbol "x"])))));

eval_print(
   TmApplication(
      TmApplication(
         TmSymbol "fix",
         [
            TmLambda(
               ["ft"],
               TmLambda(
                  ["k"],
                  TmIf(
                     TmApplication(TmSymbol "=", [TmSymbol "k", TmInt 1]),
                     TmInt 1,
                     TmApplication(
                        TmSymbol "*",
                        [
                           TmSymbol "k",
                           TmApplication(TmSymbol "ft", [TmApplication(TmSymbol "-", [TmSymbol "k", TmInt 1])])
                        ]))))
         ]),
      [TmInt 10]));

(* 4.1.7 - The Metacircular Evaluator - Separating Syntactic Analysis from Execution *)

fun eval (exp, env) = analyze exp env

and analyze (TmUnit                ) = (fn env => ValUnit)
  | analyze (TmBool   exp          ) = (fn env => ValBool   exp)
  | analyze (TmInt    exp          ) = (fn env => ValInt    exp)
  | analyze (TmReal   exp          ) = (fn env => ValReal   exp)
  | analyze (TmString exp          ) = (fn env => ValString exp)
  | analyze (TmQuoted exp          ) = (fn env => ValQuoted exp)
  | analyze (TmIf(exp, e1, e2)     ) = let
                                          val pproc = analyze exp
                                          val cproc = analyze e1
                                          val aproc = analyze e2
                                       in
                                          fn env => case pproc env of ValBool true => cproc env | _ => aproc env
                                       end
  | analyze (TmCond exp            ) = analyze(cond2if exp)
  | analyze (TmBegin exp           ) = let val aprocs = map analyze exp in fn env => foldl (fn (x, _) => x env) ValUnit aprocs end
  | analyze (TmSymbol exp          ) = (fn env => lookup_variable_value(exp, env))
  | analyze (TmDefinition(e1, e2)  ) = let val vproc = analyze e2 in fn env => define_variable(e1, vproc env, env) end
  | analyze (TmAssignment(e1, e2)  ) = let val vproc = analyze e2 in fn env => set_variable_value(e1, vproc env, env) end
  | analyze (TmLambda(parms, body) ) = let val aproc = analyze body in fn env => ValClosureA(parms, aproc, env) end
  | analyze (TmApplication(f, args)) = let
                                          val fproc = analyze f
                                          val aprocs = map analyze args
                                       in
                                          fn env => execute_application(fproc env, map (fn x => x env) aprocs)
                                       end

and execute_application (ValPrimitive(sym, f), args) = f args
  | execute_application (ValClosureA(parameters, body, env), args) =
      if length parameters <> length args
         then
            if length parameters < length args
               then raise Evaluator "Too many arguments supplied"
               else raise Evaluator "Too few arguments supplied"
         else
            let
               (* create the closure environment *)
               val new_env = Frame.map() :: env
               (* pair up the parameters and arguments into a list *)
               val pairs = ListPair.zip(parameters, args)
            in
               (* push the parameters/arguments into the closure environment *)
               map (fn (x, y) => define_variable(x, y, new_env)) pairs;
               (* return the evaluated body of the closure *)
               body new_env
            end
  | execute_application (f, args) = raise Evaluator "Unknown procedure type -- APPLY"


(* repeated from above *)
val the_global_environment2 = Scheme.make_global_environment()
fun eval_print(code) =
   let
      val valx = eval(code, the_global_environment2)
   in
      print (Scheme.valueToString valx);
      print "\n";
      valx
   end;

(* fun factorial n =
      if n = 1
         then 1
         else n * factorial(n - 1) *)
eval_print(
   TmDefinition(
      "factorial",
      TmLambda(
         ["n"],
         TmIf(
            TmApplication(TmSymbol "=", [TmSymbol "n", TmInt 1]),
               TmInt 1,
               TmApplication(
                  TmSymbol "*",
                  [
                     TmSymbol "n",
                     TmApplication(TmSymbol "factorial", [TmApplication(TmSymbol "-", [TmSymbol "n", TmInt 1])])
                  ])))));

(* factorial 5 *)
eval_print(TmApplication(TmSymbol "factorial", [TmInt 5]));


(* 4.2.1 - Variations on a Scheme -- Lazy Evaluation - Normal Order and Applicative Order *)

fun try (a, b) =
   if a = 0
      then 1
      else b

fun unless (condition, usual_value, exceptional_value) =
   if condition
      then exceptional_value
      else usual_value

(* Exercise 4.25 *)
fun factorial n =
   unless(n = 1, lazy n * factorial(n-1), 1)


(* 4.2.2 - Variations on a Scheme -- Lazy Evaluation - An Interpreter with Lazy Evaluation *)

fun eval (TmUnit                , env) = ValUnit
  | eval (TmBool   exp          , env) = ValBool   exp
  | eval (TmInt    exp          , env) = ValInt    exp
  | eval (TmReal   exp          , env) = ValReal   exp
  | eval (TmString exp          , env) = ValString exp
  | eval (TmQuoted exp          , env) = ValQuoted exp
  | eval (TmIf(exp, e1, e2)     , env) = (case eval(exp, env) of ValBool true => eval(e1, env) | _ => eval(e2, env))
  | eval (TmCond exp            , env) = eval(cond2if exp, env)
  | eval (TmBegin exp           , env) = foldl (fn (x, _) => eval(x, env)) ValUnit exp
  | eval (TmSymbol exp          , env) = lookup_variable_value(exp, env)
  | eval (TmDefinition(e1, e2)  , env) = define_variable(e1, eval(e2, env), env)
  | eval (TmAssignment(e1, e2)  , env) = set_variable_value(e1, eval(e2, env), env)
  | eval (TmLambda(parms, body) , env) = ValClosure(parms, body, env)
  | eval (TmApplication(f, args), env) = apply(eval(f, env), map (fn x => lazy eval(x, env)) args)

and apply (ValPrimitive(sym, f), args) = f(args)
  | apply (ValClosure(parameters, body, env), args) =
      if length parameters <> length args
         then
            if length parameters < length args
               then raise Evaluator "Too many arguments supplied"
               else raise Evaluator "Too few arguments supplied"
         else
            let
               (* create the closure environment *)
               val new_env = Frame.map() :: env
               (* pair up the parameters and arguments into a list *)
               val pairs = ListPair.zip(parameters, args)
            in
               (* push the parameters/arguments into the closure environment *)
               map (fn (x, y) => define_variable(x, y, new_env)) pairs;
               (* evaluate the body of the closure *)
               eval(body, new_env)
            end
  | apply (f, args) = raise Evaluator "Unknown procedure type -- APPLY"

val the_global_environment = Scheme.make_global_environment()

fun eval_print(code) =
   let
      val valx = eval(code, the_global_environment)
   in
      print (Scheme.valueToString valx);
      print "\n";
      valx
   end;


(* fun lazy unless (condition, usual_value, exceptional_value) =
      if condition
         then exceptional_value
         else usual_value *)
eval_print(
   TmDefinition(
      "unless",
      TmLambda(
         ["condition", "usual_value", "exceptional_value"],
         TmIf(
            TmSymbol "condition",
               TmSymbol "usual_value",
               TmSymbol "exceptional_value"))));

(* fun test() = unless(1 = 1, true, (print "whoops\n"; false) ); *)
eval_print(TmApplication(TmSymbol "unless",
   [
      TmBool true,
      TmBool true,
      TmBegin([TmApplication(TmSymbol "display", [TmString "whoops\n"]), TmBool false])
   ]));

(* fun try (a, b) =
     if a = 0 then 1 else b; *)
eval_print(
   TmDefinition(
      "try",
      TmLambda(
         ["a", "b"],
         TmIf(
            TmApplication(TmSymbol "=", [TmSymbol "a", TmInt 0]),
               TmSymbol "a",
               TmSymbol "b"))));

(* try(0, lazy 1 div 0); *)
eval_print(TmApplication(TmSymbol "try",
   [
      TmInt 0,
      TmApplication(TmSymbol "/", [TmInt 1, TmInt 0])
   ]));


(* Exercise 4.27 *)
(* val count = ref 0 *)
eval_print(TmDefinition("count", TmInt 0));

(* fun id x = ( count := !count + 1; x) *)
eval_print(
   TmDefinition(
      "id",
      TmLambda(
         ["x"],
         TmBegin(
            [
               TmAssignment("count", TmApplication(TmSymbol "+", [TmSymbol "count", TmInt 1])),
               TmSymbol "x"
            ]))));

(* val w = id(id 10); *)
eval_print(
   TmDefinition(
      "w",
      TmApplication(TmSymbol "id", [TmApplication(TmSymbol "id", [TmInt 10])])));

(* !count; *)
eval_print(TmSymbol "count");

(* w; *)
eval_print(TmSymbol "w");

(* !count; *)
eval_print(TmSymbol "count");

(* Exercise 4.29 *)
(* fun square x = x * x; *)
eval_print(TmDefinition("square", TmLambda(["x"], TmApplication(TmSymbol "*", [TmSymbol "x", TmSymbol "x"]))));

(* square(id 10); *)
eval_print(TmApplication(TmSymbol "id", [TmInt 10]));

(* !count; *)
eval_print(TmSymbol "count");

(* Exercise 4.30 *)
(* PART A *)
fun for_each (proc, nil) = ()
  | for_each (proc, x::xs) = ( proc x; for_each(proc, xs)  );
for_each(fn x => print(Int.toString x ^ "\n"), [57, 321, 88]);

(* PART B *)
fun p1 x = (x := !x@[2]; !x)
fun p2 x =
   let
      fun p e = ( e; x )
   in
      x := !x@[2];
      p(x)
   end
