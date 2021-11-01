type ('a, 'b) app   
   
module Term = struct
  module Var =
    struct
      type anchor = int list
      type env = int
      type scope = int
      type t =
        { anchor        : anchor
        ; env           : env
        ; scope         : scope
        ; index         : int
        ; mutable subst : Obj.t option
        ; constraints   : Obj.t list
        }
        
      let global_anchor = [-8]
                          
      let make ~env ~scope index = {
          anchor      = global_anchor
        ; env
        ; scope
        ; index
        ; subst       = None
        ; constraints = []
        }
                                 
      let dummy =
        let env = 0 and scope = 0 in make ~env ~scope 0
                                   
      let reify : ('a -> 'b) -> t -> int * 'b list =
        fun f {index; constraints} -> (index, List.map (fun x -> f (Obj.obj x)) constraints)
    end
  
  let var_tag, var_size =
    let dummy = Obj.repr Var.dummy in
    Obj.tag dummy, Obj.size dummy

  let var : 'a -> Var.t option = fun x ->
    let x' = Obj.repr x in
    if  Obj.is_block x' && Obj.size x' = var_size && Obj.tag x' = var_tag &&
          let anchored : Obj.t -> bool =
            fun y -> Obj.field y 0 == Obj.repr Var.global_anchor
          in  anchored x'
    then Some ((Obj.obj x') : Var.t)
    else None
    
end

module VarEnv = struct

  type t = {anchor : Term.Var.env; mutable next : int}

  let last_anchor = ref 11
  let first_var = 10
              
  let empty : unit -> t =
    function () ->
      incr last_anchor;
      {anchor = !last_anchor; next = first_var}
    
  let check : t -> Term.Var.t -> bool = fun env v -> (env.anchor = v.Term.Var.env)

  let check_exn env v =
    if check env v then () else failwith "OCanren fatal (Env.check) : wrong ennvironment"

  let var : t -> 'a -> Term.Var.t option = fun env x ->
    match Term.var x with
    | (Some v) as res -> check_exn env v; res
    | None            -> None
                       
end

type ('a, 'b) injected;;

module Logic = struct
  
  type 'a logic = Var of int * 'a logic list | Value of 'a;;              

  let rec fmap : ('a -> 'b) -> 'a logic -> 'b logic = fun f ->
    function
    | Var(i, cs) -> Var(i, List.map (fmap f) cs)
    | Value x -> Value (f x);;
end;;

open Logic;;

(* OCanren hinges on the machine representation of OCaml values, therefore 
   the language of OCaml runtime representation of values, whose vocabulary
   contains words like  "immediate value", "block", "pointer", "(block) header", 
   "(block) tag", "(block) size" and "(block) field", is the language to reason
   about OCanren *)

(* non-traversing reifier *)
              
let rec reify : VarEnv.t -> ('a, 'a logic) injected -> 'a logic = fun env x ->
  match VarEnv.var env x with
  | Some v -> let i, cs = Term.Var.reify (reify env) v in Var (i, cs)
  | None   -> Value (Obj.magic x)

(* Reification is an identity-preserving transformation of representation of logic terms. *)
                
(* The match case of "reify" is "None" 
   iff  VarEnv.var returns "None" 
   iff Term.var returns "None" 
   iff x is physically not a variable.  

   The match case is "Some v" 
   iff x is physically a variable that agrees with env *)

(* If x is not physically a variable then it is wrapped and returnd as is, without
   traversing  its structure.  If x is physically a variable that agrees with the env,
   then its index is returned, together with a list of similarly reified items
   to which x must not equal. *)                

(* Create an env and a matching variable for some test of the non-traversing reifier *)

let my_env = VarEnv.empty ();;
let my_var = Term.Var.make ~env:12 ~scope:0 12345;;

(* reguard my_var as an integer variable *)

reify my_env ((Obj.magic my_var) : (int, int logic) injected);;

(* reguard my_var as a bool variable *)

reify my_env ((Obj.magic my_var) : (bool, bool logic) injected);;

module Foo = struct
  type 'a foo = Foo of 'a | Bar of 'a * 'a;;
  
  type fOO ;;(* brand of foo *)
     
  external inj : 'a foo -> ('a, fOO) app = "%identity";;
  external prj : ('a, fOO) app -> 'a foo = "%identity";; 
  
  let fmap' : ('a -> 'b) -> 'a foo -> 'b foo =
    fun f -> function Foo a -> Foo (f a) | Bar(a,b) -> Bar(f a, f b);;
  
  let fmap : ('a -> 'b) -> ('a, fOO) app -> ('b, fOO) app =
    fun f x -> inj @@ fmap' f @@ prj x;;
end;;
           
module Bar = struct   
  type 'a bar = Ali of 'a | Baba of 'a * 'a * 'a;;

  type bAR ;;(* brand *)

  external inj  : 'a bar -> ('a, bAR) app = "%identity";;
  external prj  : ('a, bAR) app -> 'a bar = "%identity";;
  
  let fmap' : ('a -> 'b) -> 'a bar -> 'b bar =
  fun f -> function Ali a -> Ali (f a) | Baba(a,b,c) -> Baba(f a, f b, f c);;
  
  let fmap : ('a -> 'b) -> ('a, bAR) app -> ('b, bAR) app =
    fun f x -> inj @@ fmap' f @@ prj x;;    
end;;

open Foo;;
open Bar;;

(* the non-traversing reifier is inadequate when variables are inside a structure:
   such variables are not transformed *)

reify my_env ((Obj.magic @@ Foo my_var) : (int foo, int foo logic) injected);;
reify my_env ((Obj.magic @@ Bar(my_var,my_var)) : (int foo, int foo logic) injected);;

(* we need a traversing reifier *)

(* traversing reifier for unary type constructors; higher-kinded polymorphic *)

let rec reify1 : env:VarEnv.t                                      ->
                 fmap:(('a -> 'b) -> ('a, 't) app -> ('b, 't) app) ->
                 reifier:(('c, 'd) injected -> 'd)                 ->
                 ((('c, 't) app, ('d, 't) app logic) injected)     ->
                 ('d, 't) app logic                                 =
  fun ~env ~fmap ~reifier v ->
        match VarEnv.var env v with
        | None -> Value (fmap reifier (Obj.magic v))
        | Some x -> let i,cs = Term.Var.reify (reify1 ~env ~fmap ~reifier) x in Var(i,cs);;


Logic.fmap Foo.prj @@ reify1 ~env:my_env ~fmap:Foo.fmap ~reifier:(reify my_env)
                        (Obj.magic @@ Foo my_var);;


Logic.fmap Foo.prj @@ reify1 ~env:my_env ~fmap:Foo.fmap ~reifier:(reify my_env)
                        (Obj.magic @@  Bar(my_var,my_var));;


let tconv = fun x -> Logic.fmap Foo.prj  @@ Logic.fmap (Foo.fmap (Logic.fmap Bar.prj)) x in
    tconv @@  reify1
                ~env:my_env
                ~fmap:Foo.fmap
                ~reifier:(reify1
                            ~env:my_env
                            ~fmap:Bar.fmap
                            ~reifier:(reify my_env))
                (Obj.magic @@  Bar(Obj.magic my_var, Ali(Obj.magic my_var)));;

(* How "tconv" is fabricated: 

GOAL : (('a, bAR) app logic, fOO) app logic -> 
       'a bar logic foo logic

Foo.prj : ('a, fOO) app -> 'a foo

(F1) Logic.fmap Foo.prj : ('a, fOO) app logic -> 'a foo logic

------------------------: ('a bar logic, fOO) app logic -> 'a bar logic foo logic

Bar.prj : ('a, bAR) app -> 'a bar

Logic.fmap Bar.prj : ('a, bAR) app logic -> 'a bar logic

Foo.fmap (Logic.fmap Bar.prj) : (('a, bAR) app logic, fOO) app -> ('a bar logic, fOO) app

(F2) Logic.fmap (Foo.fmap (Logic.fmap Bar.prj)) : (('a, bAR) app logic, fOO) app logic -> ('a bar logic, fOO) app logic

fun x -> F1 (F2 x)

*)

let rec reify2 :
          env:VarEnv.t                                                    ->
          fmap:(('t1 -> 't3)               ->
                ('t2 -> 't4)               ->
                ('t1, ('t2, 't) app) app   ->
                ('t3, ('t4, 't) app) app)                                 ->
          reifier1:(('a, 'c) injected -> 'c)                              ->
          reifier2:(('b, 'd) injected -> 'd)                              ->
          (('a, ('b, 't) app) app, ('c, ('d, 't) app) app logic) injected ->
          ('c, ('d, 't) app) app logic                                     =
  fun ~env ~fmap ~reifier1 ~reifier2 v ->
        match VarEnv.var env v with
        | None -> Value (fmap reifier1 reifier2 (Obj.magic v))
        | Some x -> let i,cs = Term.Var.reify (reify2 ~env ~fmap ~reifier1 ~reifier2) x
                    in Var(i,cs);;


module MyList = struct
  
  type ('a, 'b) t = Nil | Cons of 'a * 'b;;

  (* brand for t *)
  type b;;

  external inj : ('a, 'c) t -> ('a, ('c, b) app) app = "%identity"                          
  external prj : ('a, ('c, b) app) app -> ('a, 'c) t = "%identity"                          

  let fmap' : ('a -> 'b) -> ('c -> 'd) ->  ('a, 'c) t -> ('b, 'd) t =
  fun f g -> function Nil -> Nil | Cons(a,b) -> Cons(f a, g b);;
  
  let fmap : ('a -> 'b) -> ('c -> 'd) ->  ('a, ('c, b) app) app ->  ('b, ('d, b) app) app =
    fun f g x -> inj @@ fmap' f g @@ prj x;;   
  
end;;
        
