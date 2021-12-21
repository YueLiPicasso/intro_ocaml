open Eucpp
open Eucpp.Core    
open Type

(* Reify variables *)

let _ = print_string @@ match Reifier.(apply reify @@ runrt ()) with
  | Value _ -> "failed\n"
  | Var _ -> "PASSED\n"
and _ = print_string @@ match Reifier.(apply reify @@ runfr ()) with
  | Value _ -> "failed\n"
  | Var _ -> "PASSED\n"
and _ = print_string @@
  match Reifier.(apply reify @@ run (fun _ -> Env.return @@ inj 44)) with
  | Value 44 -> "PASSED\n"
  | _ -> "failed\n"

let _ =
  let runaway : int ilogic ref = ref (Obj.magic ()) in  
  let state_ok = run (fun v -> runaway := v; Env.return v) in
  try Reifier.(apply reify @@ run (fun _ -> Env.return !runaway))   
               with
               | Var.Scope_violation _ -> print_string "PASSED\n";
                                          Reifier.(apply reify state_ok)
(* test fresh number generator *)
                                            
let _ = 
  print_string @@ Stdlib.List.fold_right
    (fun v s ->
       s ^ "(" ^
       string_of_int ((Obj.magic (Var.index v)) : int)
       ^
       " , "
       ^
       string_of_int ((Obj.magic (Var.env v))   : int)
       ^
       ")\n")
    Var.[fresh (fresh_env ());fresh (fresh_env ());
         fresh (fresh_env ());
         fresh (fresh_env ());fresh (fresh_env ());
         fresh (fresh_env ())]
    ""
(* [expr1 ; expr2; expr3 ; ... ; exprn] evalutation order 
   exprn , ..., expr3, expe2, expr1. *)

(* reify options *)

let _= print_string @@ 
  let v : 'a Core.logic Option.logic =
    Reifier.apply (Option.reify Reifier.reify) (runrt()) in
  match v with
  | Var _ -> "PASSED\n"
  | _ -> "failed\n"

let _ = print_string @@
  let tm : 'a Core.logic Option.logic =
    Reifier.apply (Option.reify Reifier.reify)
      (run (fun v -> Env.return (inj (Some v))))
  in
  match tm with
  |  Value (Some (Var _)) -> "PASSED\n"
  | _ -> "failed\n"
    
let _ = print_string @@
  let tm : int Core.logic Option.logic =
    Reifier.apply (Option.reify Reifier.reify)
      (run (fun _ -> Env.return (inj (Some (inj 49)))))
  in
  match tm with
  |  Value (Some (Value 49)) -> "PASSED\n"
  | _ -> "failed\n"

let _ = print_string @@
  let tm : bool Core.logic Option.logic Option.logic  =
    Reifier.apply (Option.reify (Option.reify Reifier.reify))
      (run (fun _ -> Env.return @@ inj (Some (inj (Some (inj true))))))
  in
  match tm with
  |  Value (Some (Value Option.(Some (Value true)))) -> "PASSED\n"
  | _ -> "failed\n"


(* Reify List *)

let _ = print_string @@
  let tm = Reifier.apply (List.reify Reifier.reify) (runrt()) in
  match tm with
  | Var _ -> "PASSED\n"
  | _ -> "failed\n"


let _ = print_string @@
  let tm = Reifier.apply (List.reify Reifier.reify)
      (run (fun v -> Env.return (inj List.(Cons(v, inj Nil))))) in
  match tm with
  | Value (Cons (Var _, Value Nil)) -> "PASSED\n"
  | _ -> "failed\n"

let _ = print_string @@
  let tm = Reifier.apply (List.reify Reifier.reify)
      (run (fun v -> Env.return
               (inj List.(Cons(v,
                               inj (Cons(v, (inj Nil)))))))) in
  match (tm : 'a Core.logic List.logic) with
  | Value(Cons(Var _, Value (Cons (Var _, Value Nil)))) -> "PASSED\n"
  | _ -> "failed\n"

let _ = print_string @@
  let tm = Reifier.apply (List.reify Reifier.reify)
      (run (fun v -> Env.return
               (inj List.(Cons(inj 5,
                               inj (Cons(inj 6, v))))))) in
  match (tm : int Core.logic List.logic) with
  | Value(Cons(Value 5, Value (Cons (Value 6, Var _)))) -> "PASSED\n"
  | _ -> "failed\n"

let _ = print_string @@
  let tm = Reifier.apply (List.reify (Option.reify Reifier.reify))
      (run (fun v -> Env.return
               (inj List.(Cons(inj (Some (inj 5)),
                               inj (Cons(v, (inj Nil)))))))) in
  match (tm : int Core.logic Option.logic List.logic) with
  | Value(Cons(Value(Some(Value 5)), Value (Cons (Var _, Value Nil))))
    -> "PASSED\n"
  | _ -> "failed\n"


let _ = print_string @@
  let tm = Reifier.apply (List.reify (Option.reify Reifier.reify))
      (run (fun v -> Env.return
               (inj List.(Cons(inj (Some v),
                               inj (Cons(inj None, (inj Nil)))))))) in
  match (tm : int Core.logic Option.logic List.logic) with
  | Value(Cons(Value(Some(Var _)), Value (Cons (Value None, Value Nil))))
    -> "PASSED\n"
  | _ -> "failed\n"

(* 
This has type error --- good ! (v get incompatiable types)

let _ = print_string @@
  let tm = Reifier.apply (List.reify (Option.reify Reifier.reify))
      (run (fun v -> Env.return
               (inj List.(Cons(inj (Some v),
                               inj (Cons(v, (inj Nil)))))))) in
  match (tm : int Core.logic Option.logic List.logic) with
  | Value(Cons(Value(Some(Var _)), Value (Cons (Value None, Value Nil))))
    -> "PASSED\n"
  | _ -> "failed\n"

How to get two vars of different but correct type?

*)

