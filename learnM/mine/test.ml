open Printf
open Core

let _ = match Reifier.(apply reify @@ runrt ()) with
  | Value _ -> printf "failed\n"
  | Var _ -> printf "PASSED\n"

let _ = match Reifier.(apply reify @@ runfr ()) with
  | Value _ -> printf "failed\n"
  | Var _ -> printf "PASSED\n"

let _ = match Reifier.(apply reify @@ run (fun v -> Env.return @@ inj 44)) with
  | Value 44 -> printf "PASSED\n"
  | _ ->  printf "failed\n"

let _ =        
  let runaway : int ilogic ref = ref (Obj.magic ()) in  
  let state_ok = run (fun v -> runaway := v; Env.return v) in
  try Reifier.(apply reify @@ run (fun v -> Env.return !runaway))   
               with
               | Var.Scope_violation _ -> printf "PASSED\n";
                                          Reifier.(apply reify state_ok)
                                         
let _ = print_string @@ List.fold_right
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
