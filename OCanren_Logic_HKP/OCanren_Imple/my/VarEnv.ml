(* Note that both Term.Var.t and VarEnv.t are records that have a field "anchor". 
   The difference is that the anchor in Term.Var.t is a "global anchor" that is taken
   to determine if something is a variable at all; however, the anchor in VarEnv.t is 
   an "environment anchor" against which a variable is checked to see if it belongs to
   a certain environment. *)

type t = {anchor : Term.Var.env; mutable next : int}

let last_anchor = ref 11
let first_var = 10

(* Each call of "empty" creates a fresh record denoting a variable environment and with 
   a fresh environment anchor. *)
              
let empty : unit -> t =
  function () ->
    incr last_anchor;
    {anchor = !last_anchor; next = first_var}

(* Given a variable and an environment, we want to check if the former belongs to the
   latter. This is done by comparing the "anchor" field of the environment with the
   "environment" field of the variable. A variable is said to belong to an environment if 
   the "env" field of the former equals the "anchor" field of the latter. *)
    
let check : t -> Term.Var.t -> bool = fun env v -> (env.anchor = v.Term.Var.env)

let check_exn env v =
  if check env v then () else failwith "OCanren fatal (Env.check) : wrong ennvironment"

(* polymorphic variable test with environment: like Term.var but additionally makes sure
   that if it is a variable then it also belongs to the given environment. *)
  
let var : t -> 'a -> Term.Var.t option = fun env x ->
  match Term.var x with
  | (Some v) as res -> check_exn env v; res
  | None            -> None
                         
                         
                     
                 
                  
