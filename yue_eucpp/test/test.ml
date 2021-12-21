open Eucpp
open Eucpp.Core    
open Type

let _ = print_string @@ match Reifier.(apply reify @@ runrt ()) with
  | Value _ -> "failed\n"
  | Var _ -> "PASSED\n"
and _ = print_string @@ match Reifier.(apply reify @@ runfr ()) with
  | Value _ -> "failed\n"
  | Var _ -> "PASSED\n"
and _ = print_string @@ match Reifier.(apply reify @@ run (fun _ -> Env.return @@ inj 44)) with
  | Value 44 -> "PASSED\n"
  | _ -> "failed\n"

let _ =
  let runaway : int ilogic ref = ref (Obj.magic ()) in  
  let state_ok = run (fun v -> runaway := v; Env.return v) in
  try Reifier.(apply reify @@ run (fun _ -> Env.return !runaway))   
               with
               | Var.Scope_violation _ -> print_string "PASSED\n";
                                          Reifier.(apply reify state_ok)
                                         
let _ = 
  print_string @@ List.fold_right
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



let _= print_string @@ 
  let v : int Core.logic Option.logic =
    Reifier.apply (Option.reify Reifier.reify) (runrt()) in
  match v with
  | Var _ -> "PASSED\n"
  | _ -> "failed\n"


(* 

ra: reifier for the argument type 'a of 'a option
env: variable environment 

Option.reify ra env = fun x ->
        match (Reifier.reify env) x with
        | Var v   -> Var v
        | Value t -> Value (Option.fmap (ra env) t)
   
*)
