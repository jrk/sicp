(* SICP Chapter #04 Examples in O'Caml *)

(* Utility function *)
let rec listpair_zip a b =
   match a, b with
    | x::xs, y::ys -> (x, y) :: listpair_zip xs ys
    | [], [] -> []
    | _ -> raise Not_found

(* 4.1.2 - The Metacircular Evaluator - Representing Expressions *)

(* Note: the parser will need to desugar the function defines of the form:
 *    let foo x y = x + y        into  val foo = (fun x -> fun y -> x + y)
 *    (define (foo x y) (+ x y)) into  (define foo (lambda (x y) (+ x y)))
 * Note: the parser should also change the cond else to expression (TmBool true)
 *)
exception Evaluator of string

module Symbol =
   struct
      type t = string
      let compare = String.compare
      let toString s = s
   end

type term =
     TmUnit
   | TmBool        of bool
   | TmInt         of int
   | TmReal        of float
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
   | ValReal       of float
   | ValString     of string
   | ValTuple      of value * value
   | ValQuoted     of term
   | ValSymbol     of Symbol.t
   | ValPrimitive  of Symbol.t * (value list -> value)
   | ValClosure    of Symbol.t list * term * environment
   | ValClosureA   of Symbol.t list * (environment -> value) * environment       (* Closure used for analyze *)

and environment = (Symbol.t, value) Hashtbl.t list


(* 4.1.3 - The Metacircular Evaluator - Evaluator Data Structures *)

let rec lookup_variable_value env var =
   match env with
    | frame::enclosing_environment ->
         if Hashtbl.mem frame var
            then Hashtbl.find frame var
            else lookup_variable_value enclosing_environment var
    | [] ->
         begin
            print_string (var ^ "\n");
            raise (Evaluator("Unbound variable " ^ (Symbol.toString var)))
         end

let rec set_variable_value env var valx =
   match env with
    | frame::enclosing_environment ->
         if Hashtbl.mem frame var
            then ( Hashtbl.replace frame var valx; valx )
            else set_variable_value enclosing_environment var valx
    | [] -> raise (Evaluator("Unbound variable -- SET! " ^ Symbol.toString var))

let define_variable env var valx =
   match env with
      | frame::_ -> ( Hashtbl.add frame var valx; valx )
      | [] -> raise (Evaluator("Empty Environment " ^ Symbol.toString var))

let rec cond2if = function
 | (pred, exp)::xs -> TmIf(pred, exp, cond2if xs)
 | [] -> TmUnit

(* primitive implementations for source language *)
module type LANGUAGE =
   sig
      val make_global_environment : unit -> (Symbol.t, value) Hashtbl.t list
      val valueToString           : value -> string
   end

module Scheme : LANGUAGE =
   struct
      let rec valueToString = function
       | ValUnit             -> "unit"
       | ValBool v           -> string_of_bool v
       | ValInt v            -> string_of_int v
       | ValReal v           -> string_of_float v
       | ValString v         -> "\"" ^ v ^ "\""
       | ValTuple(x, y)      -> "Pair(" ^ valueToString x ^ ", " ^ valueToString y ^ ")"
       | ValQuoted v         -> "Quote"
       | ValSymbol v         -> v
       | ValPrimitive (v, f) -> "<Primitive: " ^ Symbol.toString v ^ ">"
       | ValClosure _        -> "<Closure>"
       | ValClosureA _       -> "<Closure>"

      let isValTrue = function
       | ValBool true -> true
       | _ -> false

      let rec primitive_eq args =
         let rec compeq x y =
            match x, y with
               ValBool a, ValBool b           -> a = b
             | ValInt a, ValInt b             -> a = b
             | ValInt a, b                    -> compeq (ValReal(float_of_int a)) b
             | ValReal a, ValReal b           -> a == b
             | ValString a, ValString b       -> a = b
             | ValUnit, ValUnit               -> true
             | ValUnit, ValTuple _            -> false
             | ValTuple _, ValUnit            -> false
             | ValTuple(a, b), ValTuple(c, d) -> compeq a c && compeq b d
             | _ -> raise (Evaluator "Invalid Compare")
         in
            match List.length args with
               0 -> raise (Evaluator "Invalid Number of arguments for compare")
             | 1 -> raise (Evaluator "Invalid Number of arguments for compare")
             | 2 -> ValBool(compeq (List.hd args) (List.hd(List.tl args)))
             | _ -> ValBool(compeq (List.hd args) (List.hd(List.tl args)) &&
                            isValTrue(primitive_eq(List.tl args)))

      let primitive_neq args =
         ValBool(not(isValTrue(primitive_eq args)))

      let rec primitive_gt args =
         let rec compgt x y =
            match x, y with
             | ValInt a, ValInt b           -> a > b
             | ValInt a, b                  -> compgt (ValReal(float_of_int a)) b
             | ValReal a, ValReal b         -> a > b
             | _ -> raise (Evaluator "Invalid Compare")
         in
            match List.length args with
               0 -> raise (Evaluator "Invalid Number of arguments for compare")
             | 1 -> raise (Evaluator "Invalid Number of arguments for compare")
             | 2 -> ValBool(compgt (List.hd args) (List.hd(List.tl args)))
             | _ -> ValBool(compgt (List.hd args) (List.hd(List.tl args)) &&
                            isValTrue(primitive_gt(List.tl args)))

      let primitive_lt args =
         ValBool(not(isValTrue(primitive_eq args)) && not(isValTrue(primitive_gt args)))

      let primitive_gte args =
         ValBool(isValTrue(primitive_eq args) || isValTrue(primitive_gt args))

      let primitive_lte args =
         ValBool(isValTrue(primitive_eq args) || not(isValTrue(primitive_gt args)))

      let rec primitive_plus args =
         match args with
          | [] -> ValInt 0
          | ValInt x :: xs ->
               begin
                  match primitive_plus xs with
                   | ValInt y -> ValInt(x + y)
                   | ValReal y -> ValReal((float_of_int x) +. y)
                   | _ -> raise (Evaluator "Unexpected error for plus")
               end
          | ValReal x :: xs ->
               begin
                  match primitive_plus xs with
                   | ValInt y -> ValReal(x +. float_of_int y)
                   | ValReal y -> ValReal(x +. y)
                   | _ -> raise (Evaluator "Unexpected error for plus")
               end
          | _ -> raise (Evaluator "Invalid argument for plus")

      let primitive_minus args =
         match args with
          | [] -> raise (Evaluator "Invalid argument for minus")
          | ValInt x :: xs ->
               begin
                  match primitive_plus xs with
                   | ValInt y -> ValInt(x - y)
                   | ValReal y -> ValReal(float_of_int x -. y)
                   | _ -> raise (Evaluator "Unexpected error for minus")
               end
          | ValReal x :: xs ->
               begin
                  match primitive_plus xs with
                     ValInt y -> ValReal(x -. float_of_int y)
                   | ValReal y -> ValReal(x -. y)
                   | _ -> raise (Evaluator "Unexpected error for minus")
               end
          | _ -> raise (Evaluator "Invalid argument for plus")

      let primitive_multiply args =
         match args with
          | [] -> ValInt 1
          | ValInt x::[] -> ValInt x
          | ValReal x::[] -> ValReal x
          | ValInt x :: xs ->
               begin
                  match primitive_plus xs with
                   | ValInt y -> ValInt(x * y)
                   | ValReal y -> ValReal(float_of_int x *. y)
                   | _ -> raise (Evaluator "Unexpected error for multiply")
               end
          | ValReal x :: xs ->
               begin
                  match primitive_plus xs with
                   | ValInt y -> ValReal(x *. float_of_int y)
                   | ValReal y -> ValReal(x *. y)
                   | _ -> raise (Evaluator "Unexpected error for multiply")
               end
          | _ -> raise (Evaluator "Invalid argument for multiply")

      (* Note: not currently supporting scheme's rational fractions *)
      let primitive_divide args =
         match args with
          | [] -> raise (Evaluator "Invalid argument for minus")
          | ValInt x :: [] -> ValReal(1.0 /. float_of_int x)
          | ValReal x :: [] -> ValReal(1.0 /. x)
          | ValInt x :: xs ->
               begin
                  match primitive_multiply xs with
                     ValInt 0 -> raise (Evaluator "Divide by zero error")
                   | ValReal 0.0 -> raise (Evaluator "Divide by zero error")
                   | ValInt y -> ValReal(float_of_int x /. float_of_int y)
                   | ValReal y -> ValReal(float_of_int x /. y)
                   | _ -> raise (Evaluator "Unexpected error for divide")
               end
          | ValReal x :: xs ->
               begin
                  match primitive_multiply xs with
                   | ValInt y -> ValReal(x /. float_of_int y)
                   | ValReal y -> ValReal(x /. y)
                   | _ -> raise (Evaluator "Unexpected error for divide")
               end
          | _ -> raise (Evaluator "Invalid argument for divide")

      let primitive_null = function
       | [ValUnit] -> ValBool true
       | _::_ -> ValBool false
       | [] -> ValBool false

      let primitive_cons = function
       | car::cdr::[] -> ValTuple(car, cdr)
       | _ -> raise (Evaluator "Invalid arguments for cons")

      let primitive_car = function
       | [ValTuple(car, cdr)] -> car
       | _ -> raise (Evaluator "Invalid arguments for car")

      let primitive_cdr = function
       | [ValTuple(car, cdr)] -> cdr
       | _ -> raise (Evaluator "Invalid arguments for cdr")

      let rec primitive_and = function
       | [] -> ValBool true
       | x::[] -> x
       | ValBool false :: _ -> ValBool false
       | x::xs -> primitive_and xs

      let rec primitive_or = function
       | [] -> ValBool false
       | x::[] -> x
       | ValBool true :: _ -> ValBool true
       | ValBool false :: xs -> primitive_or xs
       | x::xs -> x

      let primitive_not = function
       | [ValBool false] -> ValBool true
       | [x] -> ValBool false
       | _ -> raise (Evaluator "Invalid number of arguments for not")

      let rec primitive_display = function
       | x::[] -> ( print_string(valueToString x ^ "\n"); ValUnit )
       | x::y::[] -> primitive_display(x::[])
       | _ -> raise (Evaluator "Invalid number of arguments for display")

      let primitive_string_append args =
         let rec iter s = function
          | [] -> ValString s
          | ValString x :: xs -> iter (s ^ x) xs
          | _ -> raise (Evaluator "Invalid arguments for string-append")
         in iter "" args

      let make_global_environment () =
         let frame = Hashtbl.create 1
         in
            begin
               Hashtbl.add frame "="             (ValPrimitive("="            , primitive_eq           ));
               Hashtbl.add frame "<>"            (ValPrimitive("<>"           , primitive_neq          ));
               Hashtbl.add frame ">"             (ValPrimitive(">"            , primitive_gt           ));
               Hashtbl.add frame "<"             (ValPrimitive("<"            , primitive_lt           ));
               Hashtbl.add frame ">="            (ValPrimitive(">="           , primitive_gte          ));
               Hashtbl.add frame "<="            (ValPrimitive("<="           , primitive_lte          ));
               Hashtbl.add frame "+"             (ValPrimitive("+"            , primitive_plus         ));
               Hashtbl.add frame "-"             (ValPrimitive("-"            , primitive_minus        ));
               Hashtbl.add frame "*"             (ValPrimitive("*"            , primitive_multiply     ));
               Hashtbl.add frame "/"             (ValPrimitive("/"            , primitive_divide       ));
               Hashtbl.add frame "null?"         (ValPrimitive("null?"        , primitive_null         ));
               Hashtbl.add frame "cons"          (ValPrimitive("cons"         , primitive_cons         ));
               Hashtbl.add frame "car"           (ValPrimitive("car"          , primitive_car          ));
               Hashtbl.add frame "cdr"           (ValPrimitive("cdr"          , primitive_cdr          ));
               Hashtbl.add frame "and"           (ValPrimitive("and"          , primitive_and          ));
               Hashtbl.add frame "or"            (ValPrimitive("or"           , primitive_or           ));
               Hashtbl.add frame "not"           (ValPrimitive("not"          , primitive_not          ));
               Hashtbl.add frame "display"       (ValPrimitive("display"      , primitive_display      ));
               Hashtbl.add frame "string-append" (ValPrimitive("string-append", primitive_string_append));
               frame::[]
            end
   end


(* 4.1.1 - The Metacircular Evaluator - The Core of the Evaluator *)

let rec eval (env : environment) = function
   | TmUnit                 -> ValUnit
   | TmBool exp             -> ValBool exp
   | TmInt exp              -> ValInt exp
   | TmReal exp             -> ValReal exp
   | TmString exp           -> ValString exp
   | TmQuoted exp           -> ValQuoted exp
   | TmIf(exp, e1, e2)      -> (match eval env exp with ValBool true -> eval env e1 | _ -> eval env e2)
   | TmCond exp             -> eval env (cond2if exp)
   | TmBegin exp            -> List.fold_left (fun _ x -> eval env x) ValUnit exp
   | TmSymbol exp           -> lookup_variable_value env exp
   | TmDefinition(e1, e2)   -> define_variable env e1 (eval env e2)
   | TmAssignment(e1, e2)   -> set_variable_value env e1 (eval env e2)
   | TmLambda(parms, body)  -> ValClosure(parms, body, env)
   | TmApplication(f, args) -> apply (List.map (fun x -> eval env x) args) (eval env f)

and apply args = function
   | ValPrimitive(sym, f) -> f args
   | ValClosure(parameters, body, env) ->
      if List.length parameters <> List.length args
         then
            if List.length parameters < List.length args
               then raise (Evaluator "Too many arguments supplied")
               else raise (Evaluator "Too few arguments supplied")
         else
            let new_env = (Hashtbl.create 1) :: env         (* create the closure environment *)
            and pairs = listpair_zip parameters args        (* pair up the parameters and arguments into a list *)
            in
               begin
                  (* push the parameters/arguments into the closure environment *)
                  List.map (fun (x, y) -> define_variable new_env x y) pairs;
                  (* evaluate the body of the closure *)
                  eval new_env body
               end
   | f -> raise (Evaluator "Unknown procedure type -- APPLY")


(* 4.1.4 - The Metacircular Evaluator - Running the Evaluator as a Program *)

let the_global_environment = Scheme.make_global_environment()

let eval_print code =
   let valx = eval the_global_environment code
   in
      begin
         print_string (Scheme.valueToString valx);
         print_string "\n";
         valx
      end;;

(* 1 + 6 *)
eval_print(TmApplication(TmSymbol "+", [TmInt 1; TmInt 6]));;

(* 1 + (2 * 3) *)
eval_print(TmApplication(TmSymbol "+", [TmInt 1; TmApplication(TmSymbol "*", [TmInt 2; TmInt 3])]));;

(* let x = 6 *)
eval_print(TmDefinition("x", TmInt 6));;

(* (1 + x) *)
eval_print(TmApplication(TmSymbol "+", [TmInt 1; TmSymbol "x"]));;

(* let pi = 3.14 *)
eval_print(TmDefinition("pi", TmReal 3.14));;

(* 27.0 / (13.0 - pi) *)
eval_print(TmApplication(TmSymbol "/", [TmReal 27.0; TmApplication(TmSymbol "-", [TmReal 13.0; TmSymbol "pi"])]));;

(* val square = fn x -> x * x *)
eval_print(TmDefinition("square", TmLambda(["x"], TmApplication(TmSymbol "*", [TmSymbol "x"; TmSymbol "x"]))));;

(* let z = square 5.0 *)
eval_print(TmDefinition("z", TmApplication(TmSymbol "square", [TmReal 5.0])));;

(* let append xs ys =
      if xs = []
         then ys
         else List.hd xs :: append (List.tl xs) ys *)
eval_print(
   TmDefinition(
      "append",
      TmLambda(
         ["xs"; "ys"],
         TmIf(
            TmApplication(TmSymbol "=", [TmSymbol "xs"; TmUnit]),
            TmSymbol "ys",
            TmApplication(
               TmSymbol "cons",
               [
                  TmApplication(TmSymbol "car", [TmSymbol "xs"]);
                  TmApplication(
                     TmSymbol "append",
                     [
                        TmApplication(TmSymbol "cdr", [TmSymbol "xs"]);
                        TmSymbol "ys"
                     ])
               ])))));;

(* let xs = ["a", "b", "c"] *)
eval_print(
   TmDefinition(
      "xs",
      TmApplication(
         TmSymbol "cons",
         [
            TmString "a";
            TmApplication(
               TmSymbol "cons",
               [
                  TmString "b";
                  TmApplication(TmSymbol "cons", [TmString "c"; TmUnit])
               ])
         ])));;

(* let xs = ["d", "e", "f"] *)
eval_print(
   TmDefinition(
      "ys",
      TmApplication(
         TmSymbol "cons",
         [
            TmString "d";
            TmApplication(
               TmSymbol "cons",
               [
                  TmString "e";
                  TmApplication(TmSymbol "cons", [TmString "f"; TmUnit])
               ])
         ])));;

(* let zs = append(xs, ys) *)
eval_print(TmApplication(TmSymbol "append", [TmSymbol "xs"; TmSymbol "ys"]));;

(* (cond ((> x 0) x)
         ((= x 0) (display 'zero) 0)
         (else (- x)))  *)
eval_print(
   TmCond(
      [
         (TmApplication(TmSymbol ">", [TmSymbol "x"; TmInt 0]), TmSymbol "x");
         (TmApplication(TmSymbol "=", [TmSymbol "x"; TmInt 0]),
            TmBegin([TmApplication(TmSymbol "display", [TmString "zero"]); TmInt 0]));
         (TmBool true, TmApplication(TmSymbol "-", [TmSymbol "x"]))
      ]));;

(* if x > 0
      then x
      else
         if x = 0
           then ( print_string "zero"; 0 )
           else -x *)
eval_print(
   TmIf(
      TmApplication(TmSymbol ">", [TmSymbol "x"; TmInt 0]),
      TmSymbol "x",
      TmIf(
         TmApplication(TmSymbol "=", [TmSymbol "x"; TmInt 0]),
         TmBegin([TmApplication(TmSymbol "display", [TmString "zero"]); TmInt 0]),
         TmApplication(TmSymbol "-", [TmSymbol "x"]))));;

(* let x = 3 in
   let y = x + 2 in
   let z = x + y + 5
   in x * z *)
eval_print(
   TmApplication(
      TmLambda(
         [],
         TmBegin(
            [
               TmDefinition("x", TmInt 3);
               TmDefinition("y", TmApplication(TmSymbol "+", [TmSymbol "x"; TmInt 2]));
               TmDefinition("z", TmApplication(TmSymbol "+",
                  [TmSymbol "x"; TmApplication(TmSymbol "+", [TmSymbol "y"; TmInt 5])]));
               TmApplication(TmSymbol "*", [TmSymbol "x"; TmSymbol "z"])
            ])),
      []));;

(* The "and" is not working properly for val.
   The answer given is 5, but it should be 3.
   let x = 1;;
   let x = 3
   and y = x + 2
   in y *)
eval_print(TmDefinition("x", TmInt 1));;
eval_print(
   TmApplication(
      TmLambda(
         [],
         TmBegin(
            [
               TmDefinition("x", TmInt 3);
               TmDefinition("y", TmApplication(TmSymbol "+", [TmSymbol "x"; TmInt 2]));
               TmSymbol "y"
            ])),
      []));;

(* An extension to the eval function should address this problem:
   ((let? exp) (m-eval (let->combination exp) env))
   (define (let->combination let-exp)
     (let ((names (let-bound-variables let-exp))
           (values (let-values let-exp))
           (body (let-body let-exp)))
       (cons (list 'lambda names body) values))) *)

(* let fib n =
      let fib_iter a b count =
         if count = 0
            then b
            else fib_iter (a + b) a (count - 1)
      in fib_iter 1 0 n *)
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
                     ["a"; "b"; "count"],
                     TmIf(
                        TmApplication(TmSymbol "=", [TmSymbol "count"; TmInt 0]),
                        TmSymbol "b",
                        TmApplication(
                           TmSymbol "fib_iter",
                           [
                              TmApplication(TmSymbol "+", [TmSymbol "a"; TmSymbol "b"]);
                              TmSymbol "a";
                              TmApplication(TmSymbol "-", [TmSymbol "count"; TmInt 1])
                          ]))));
               TmApplication(TmSymbol "fib_iter", [TmInt 1; TmInt 0; TmSymbol "n"])
            ]))));;

(* fib 10 *)
eval_print(TmApplication(TmSymbol "fib", [TmInt 10]));;


(* 4.1.5 - The Metacircular Evaluator - Data as Programs *)

(* let factorial n =
      if n = 1
         then 1
         else n * factorial(n - 1) *)
eval_print(
   TmDefinition(
      "factorial",
      TmLambda(
         ["n"],
         TmIf(
            TmApplication(TmSymbol "=", [TmSymbol "n"; TmInt 1]),
               TmInt 1,
               TmApplication(
                  TmSymbol "*",
                  [
                     TmSymbol "n";
                     TmApplication(TmSymbol "factorial", [TmApplication(TmSymbol "-", [TmSymbol "n"; TmInt 1])])
                  ])))));;

(* factorial 5 *)
eval_print(TmApplication(TmSymbol "factorial", [TmInt 5]));;


(* (eval '( * 5 5) user-initial-environment) *)
eval_print(TmApplication(TmSymbol "*", [TmInt 5; TmInt 5]));;

(* Need to write a parser before I can translate this:
   (eval (cons '* (list 5 5)) user-initial-environment) *)

(* Exercise 4.15 *)
let rec run_forever () = run_forever()

let halts p q = true

exception Halted
let tryme p =
   if halts p p
      then run_forever ()
      else raise Halted;;

(* 4.1.6 - The Metacircular Evaluator - Internal Definitions *)

(* let f x =
      let isEven n =
         if n = 0
            then true
            else isOdd (n-1)
      and isOdd n =
         if n = 0
            then false
            else isEven (n-1)
      in
         begin
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
                        TmApplication(TmSymbol "=", [TmSymbol "n"; TmInt 0]),
                        TmBool true,
                        TmApplication(
                           TmSymbol "isOdd",
                           [TmApplication(TmSymbol "-", [TmSymbol "n"; TmInt 1])]))));
               TmDefinition(
                  "isOdd",
                  TmLambda(
                     ["n"],
                     TmIf(
                        TmApplication(TmSymbol "=", [TmSymbol "n"; TmInt 0]),
                        TmBool false,
                        TmApplication(
                           TmSymbol "isEven",
                           [TmApplication(TmSymbol "-", [TmSymbol "n"; TmInt 1])]))));
               TmApplication(TmSymbol "isEven", [TmSymbol "x"])
            ]))));;

eval_print(TmApplication(TmSymbol "f", [TmInt 3]));;

(* Exercise 4.19 *)
(* let a = 1 in
      let f x =
         let b = a + x in
         let a = 5
         in a + b
      in f 10 *)
eval_print(
   TmBegin(
      [
         TmDefinition("a", TmInt 1);
         TmDefinition(
            "f",
            TmLambda(
               ["x"],
               TmBegin(
                  [
                     TmDefinition("b", TmApplication(TmSymbol "+", [TmSymbol "a"; TmSymbol "x"]));
                     TmDefinition("a", TmInt 5);
                     TmApplication(TmSymbol "+", [TmSymbol "a"; TmSymbol "b"])
                  ])));
         TmApplication(TmSymbol "f", [TmInt 10])
      ]));

(* Exercise 4.20 *)
(* let factorial n =
      if n = 1
         then 1
         else n * factorial(n - 1) *)
eval_print(
   TmDefinition(
      "factorial",
      TmLambda(
         ["n"],
         TmIf(
            TmApplication(TmSymbol "=", [TmSymbol "n"; TmInt 1]),
               TmInt 1,
               TmApplication(
                  TmSymbol "*",
                  [
                     TmSymbol "n";
                     TmApplication(TmSymbol "factorial", [TmApplication(TmSymbol "-", [TmSymbol "n"; TmInt 1])])
                  ])))));;

(* Exercise 4.21 *)
(* Y Combinator in Scheme
   (fn n -> (
      (fn fact -> fact fact n)
      (fn (ft, k) ->
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
                     TmApplication(TmSymbol "=", [TmSymbol "k"; TmInt 1]),
                     TmInt 1,
                     TmApplication(
                        TmSymbol "*",
                        [
                           TmSymbol "k";
                           TmApplication(
                              TmApplication(TmSymbol "ft", [TmSymbol "ft"]),
                              [TmApplication(TmSymbol "-", [TmSymbol "k"; TmInt 1])])
                        ]))))
         ]),
      [TmInt 10]));;

(* Y Combinator in ML
   let fix f x = f (fix f) x
   let f = fix (fun ft k -> if k = 1 then 1 else k * ft(k-1)) 10; *)
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
               [TmSymbol "x"])))));;

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
                     TmApplication(TmSymbol "=", [TmSymbol "k"; TmInt 1]),
                     TmInt 1,
                     TmApplication(
                        TmSymbol "*",
                        [
                           TmSymbol "k";
                           TmApplication(TmSymbol "ft", [TmApplication(TmSymbol "-", [TmSymbol "k"; TmInt 1])])
                        ]))))
         ]),
      [TmInt 10]));;
