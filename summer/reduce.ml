open Syntax;;

(* small-step reduction semantics *)

(* check that an expression is a value *)
let rec evaluated = function
    Fun(_,_) -> true
  | u -> partial_application 0 u
and partial_application n = function
  (* n counts arguments *)
    Const c -> (c.constr || c.arity > n)
  | App(u, v) -> (evaluated v && partial_application (n + 1) u)
  | _ -> false;;


(* reduction rule for primitives *)

exception Reduce;;

let delta_bin_arith op code = function
  | App(App(Const {name = Name _; arity =2} as c,
            Const {name = Int x}),
        Const {name = Int y})
    when c = op -> int (code x y)
  | _ -> raise Reduce;;

let delta_plus = delta_bin_arith plus ( + );;
let delta_times = delta_bin_arith times ( * );;
let delta_rules = [delta_plus;delta_times];;

(* union of functions   *)
let union f g a = try g a with Reduce -> f a;;

(* delta is the union of all delta rules*)
let delta = List.fold_right union delta_rules (fun _ -> raise Reduce);; 


(* substitution *)
let rec subst x v a =
  assert (evaluated v);
  match a with
  | Var y -> if x = y then v else a
  | Fun(y, a') -> if x = y then a else Fun(y,subst x v a')
  | App(a',a'') -> App(subst x v a', subst x v a'')
  | Let (y, a', a'') -> if x = y then Let(y, subst x v a', a'')
        else Let (y, subst x v a', subst x v a'')
  | Const _ -> a;; 

(* beta reduction for let-binding and applying abstraction
   Note the call-by-value evaluation strategy *)
let beta = function
  | App (Fun(x,a), v) when evaluated v ->  subst x v a
  | Let (x, v ,a) when evaluated v -> subst x v a
  | _ -> raise Reduce;;

let top_reduce = union beta delta;;


(* evaluation using small-step reduction *)
(* evaluation context is kept implicit *)
let rec eval = function
  | App(a1,a2) ->
    let v1 = eval a1 in
    let v2 = eval a2 in
    eval_top_reduce (App(v1,v2))
  | Let (x, a1, a2) ->
    let v1 = eval a1 in
    eval_top_reduce (Let(x,v1,a2))
  | a -> eval_top_reduce a
and eval_top_reduce a =
  try eval (top_reduce a) with
    Reduce -> a;;


(* one-step reduction *)
(* the cases correspond to the syntax of an evaluation context E *)
(* E ::= [-] | E a | v E | let x = E in a *)
(* E shall be regarded as a reducible expression, where [-] is the
   only redex (among other redices) to which the 
   beta or delta rule is applicable under the call-by-value and 
   left-to-right evaluation strategy *)

let rec one_step a =
  try top_reduce a with Reduce ->
  match a with
  | App(a1,a2) when not (evaluated a1) -> App(one_step a1, a2)
  | App(a1,a2) when not (evaluated a2) -> App(a1, one_step a2)
  | Let(x,a1,a2) when not (evaluated a1) -> Let(x,one_step a1,a2)
  | _ -> raise Reduce;;


(* We can further separate an expression into a potential
   redex and its evaluation context *)

type context = expr -> expr;;

(* context fomers *)
let hole : context = fun t -> t;;
(* (appL <expr>) is a context of the form App(_ , <expr>)*)
let appL a t = App(t,a);;
let appR a t = App(a,t);;
let letL x a t = Let(x,t,a);;
(* context composing operator ( ** ),
   where e1 is outer context and e0 is sub-context 
   and a0 is an isolated redex*)
let ( ** ) (e1 : expr -> expr) ((e0,a0) : context * expr)
  = (fun (a : expr) : expr -> e1 (e0 a)), a0;;

let rec find_redex : expr -> context * expr = function
  | App(a1,a2) when not (evaluated a1) ->
    appL a2 ** find_redex a1
  | App(a1,a2) when not (evaluated a2) ->
    appR a1 ** find_redex a2
  | Let(x,a1,a2) when not (evaluated a1) ->
    letL x a2 ** find_redex a1
  | a -> hole, a;; (* a is return as a potential redex *)


(* alternative one-step reduction *)
let one_step' a = let c,t = find_redex a in c (top_reduce t);; 

(* alternative definition of eval using one_step/one_step' *)
let rec eval' a = try eval (one_step' a ) with Reduce -> a;;




(*

one_step' @@ one_step' @@ one_step' @@ one_step'  (Let("x",
          (App(Fun("x",App(App(plus, Var "x"), int 5)),
               int 3))
          , App(App(times, int 3), Var "x")));;

find_redex (int 1);;
find_redex (App(Fun("x",App(App(plus, Var "x"), int 5)),
               int 3));;
find_redex (Let("x",
          (App(Fun("x",App(App(plus, Var "x"), int 5)),
               int 3))
               , App(App(times, int 3), Var "x")));;


one_step @@ one_step @@ one_step @@ one_step @@ one_step (Let("x",
          (App(Fun("x",App(App(plus, Var "x"), int 5)),
               int 3))
          , App(App(times, int 3), Var "x")));;


one_step (App(App(plus, int 1),int 2));;
one_step (int 1);;
let ycom = App(Fun("x",App(Var "x", Var "x")),Fun("y",App(Var "y", Var "y")));;
top_reduce @@ top_reduce ycom = top_reduce ycom ;;

eval ycom;;


top_reduce (int 1);;
eval (int 1);;
eval plus;;
eval (App(plus, int 1));;
eval (App(int 1, int 1));;
eval (Let("x", App(plus, int 1), App(Var "x", int 5)));;
eval (Fun("x",App(App(plus, Var "x"), int 5)));;
top_reduce (App(Fun("x",App(App(plus, Var "x"), int 5)), int 3)) =
eval (App(Fun("x",App(App(plus, Var "x"), int 5)), int 3));;
eval (App(Fun("x",App(App(plus, Var "x"), int 5)), int 3));;
evaluated (eval (Let("x",
          (App(Fun("x",App(App(plus, Var "x"), int 5)),
               int 3))
          , App(App(times, int 3), Var "x"))));;


eval (App(Fun("x", App(Var "x", int 5)),Fun("x", App(plus, Var "x"))));;

top_reduce @@  top_reduce (App(Fun("x", App(Var "x", int 5)),Fun("x", App(plus, Var "x"))));;
evaluated @@ beta @@ beta (App(Fun("x", App(Var "x", int 5)),Fun("x", App(plus, Var "x"))));;
top_reduce (App(Fun("x",App(App(plus, Var "x"), int 5)), Var "y"));;
delta @@ beta (App(Fun("x",App(App(plus, Var "x"), int 5)), int 3));;
delta @@ beta (Let("x", App(plus, int 1), App(Var "x", int 5)));;
delta (App(App(plus, int 2), int 5));;
delta (App(App(times, int 2), int 5));;
evaluated (Fun("x", App(App(plus, int 1), Var "x" )));; (* true *)
evaluated (int 1);; (* true *)
evaluated plus;; (* true *)
evaluated (App(plus, int 1));; (* true *)
evaluated (App(App(plus, int 1), int 1 ));; (* false *)
evaluated (App(int 1, int 1));; (* true *)

*)

