#rectypes;;

(*---------------------------------------------------------*)
(*--light-weight higher-kinded polymorphic type encoding---*)
(*---------------------------------------------------------*)

type ('a, 'b) app = App of 'a * 'b

(*----------------------------------------------------*)
(*------logic variable internal representation--------*)
(*----------------------------------------------------*)

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

(*----------------------------------------------------*)
(*--------------interface of logic terms--------------*)
(*----------------------------------------------------*)              
              
type ('a, 'b) injected;;

module Logic = struct
  
  type 'a logic = Var of int * 'a logic list | Value of 'a;;              

  let rec fmap : ('a -> 'b) -> 'a logic -> 'b logic = fun f ->
    function
    | Var(i, cs) -> Var(i, List.map (fmap f) cs)
    | Value x -> Value (f x);;
end;;

open Logic;;


(*----------------------------------------------------*)
(*----------higher-kinded polymorphic reifiers--------*)
(*----------------------------------------------------*)

(* non-traversing reifier; kind is N/A here *)
              
let rec reify : VarEnv.t -> ('a, 'a logic) injected -> 'a logic = fun env x ->
  match VarEnv.var env x with
  | Some v -> let i, cs = Term.Var.reify (reify env) v in Var (i, cs)
  | None   -> Value (Obj.magic x)

(* traversing reifer for types of the form 'a t, recoded 
   as type ('a, bt) app, where bt is the brand for t. 
   It takes a sub-reifier subr for 'a. *)
            
let rec reify1 : env:VarEnv.t                                      ->
                 fmap:(('a -> 'b) -> ('a, 't) app -> ('b, 't) app) ->
                 subr:(('c, 'd) injected -> 'd)                    ->
                 ((('c, 't) app, ('d, 't) app logic) injected)     ->
                 ('d, 't) app logic                                 =
  fun ~env ~fmap ~subr v ->
  match VarEnv.var env v with
  | None -> Value (fmap subr (Obj.magic v))
  | Some x -> let i,cs = Term.Var.reify (reify1 ~env ~fmap ~subr) x in Var(i,cs);;

(* traversing reifer for types of the form ('a, 'b) t, recoded 
   as type ('a, ('b, bt) app) app, where bt is the brand for t.
   It takes sub-reifiers subr1 and subr2 for 'a and 'b respectively. *)

let rec reify2 :
          env:VarEnv.t                                                    ->
          fmap:(('t1 -> 't3)               ->
                ('t2 -> 't4)               ->
                ('t1, ('t2, 't) app) app   ->
                ('t3, ('t4, 't) app) app)                                 ->
          subr1:(('a, 'c) injected -> 'c)                                 ->
          subr2:(('b, 'd) injected -> 'd)                                 ->
          (('a, ('b, 't) app) app, ('c, ('d, 't) app) app logic) injected ->
          ('c, ('d, 't) app) app logic                                     =
  fun ~env ~fmap ~subr1 ~subr2 v ->
        match VarEnv.var env v with
        | None -> Value (fmap subr1 subr2 (Obj.magic v))
        | Some x -> let i,cs = Term.Var.reify (reify2 ~env ~fmap ~subr1 ~subr2) x
                    in Var(i,cs);;

(*----------------------------------------------------*)
(*---------------------some testss--------------------*)
(*----------------------------------------------------*)

let my_env = VarEnv.empty ();;
let my_var = Term.Var.make ~env:12 ~scope:0 12345;;


module Foo = struct
  type 'a foo = Foo of 'a | Bar of 'a * 'a;;
  
  type fOO ;;(* brand of foo *)

  external inj : 'a foo -> ('a, fOO) app = "%identity";;
  external prj : ('a, fOO) app -> 'a foo = "%identity";; 
  
  let fmap' : ('a -> 'b) -> 'a foo -> 'b foo =
    fun f -> function Foo a -> Foo (f a) | Bar(a,b) -> Bar(f a, f b);;
  
  let fmap : ('a -> 'b) -> ('a, fOO) app -> ('b, fOO) app =
    fun f x -> inj @@ fmap' f @@ prj x;;

  let reify env subr x = reify1 ~env ~fmap ~subr x;;
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

  let reify env subr x = reify1 ~env ~fmap ~subr x;;
end;;

(* traversing reifier for unary type constructors; higher-kinded polymorphic *)

Logic.fmap Foo.prj @@ Foo.reify my_env (reify my_env) (Obj.magic @@ Foo.Foo my_var);;


Logic.fmap Foo.prj @@ Foo.reify my_env (reify my_env) (Obj.magic @@  Foo.Bar(my_var,my_var));;


let tconv = fun x -> Logic.fmap Foo.prj  @@ Logic.fmap (Foo.fmap (Logic.fmap Bar.prj)) x in
    tconv @@  Foo.reify my_env (Bar.reify my_env (reify my_env))
                (Obj.magic @@  Foo.Bar(Obj.magic my_var, Bar.Ali(Obj.magic my_var)));;

module AList = struct
  
  type ('a, 'b) t = Nil | Cons of 'a * 'b;;

  (* brand for t *)
  type b;;

  external inj : ('a, 'c) t -> ('a, ('c, b) app) app = "%identity";;
    
  external prj : ('a, ('c, b) app) app -> ('a, 'c) t = "%identity" ;;                         
                                                       
  let fmap' : ('a -> 'b) -> ('c -> 'd) ->  ('a, 'c) t -> ('b, 'd) t =
      fun f g -> function Nil -> Nil | Cons(a,b) -> Cons(f a, g b);;
  
  let fmap : ('a -> 'b) -> ('c -> 'd) ->  ('a, ('c, b) app) app ->  ('b, ('d, b) app) app =
    fun f g x -> inj @@ fmap' f g @@ prj x;;
    
  module NonRec = struct
    let reify env subr1 subr2 x = reify2 ~env ~fmap ~subr1 ~subr2 x;;
  end;;
  
  module Rec = struct
    let rec reify env subr x =  NonRec.reify env subr (reify env subr) x;;
  end;;

  include NonRec;;
  (*
    external inj : (('a, 'c) t as 'c) -> (('a, ('c, b) app) app as 'c) = "%identity";;
    
    external prj : (('a, ('c, b) app) app as 'c) -> (('a, 'c) t as 'c) = "%identity";;
    
    let rec fmap' : ('a -> 'b) ->  (('a, 'c) t as 'c)  -> (('b, 'c) t as 'c) =
      fun f-> function Nil -> Nil | Cons(a,b) -> Cons(f a, fmap' f b);;
     
    let fmap : ('a -> 'b)->  (('a, ('c, b) app) app as 'c) ->  (('b, ('c, b) app) app as 'c) =
      fun f x -> inj @@ fmap' f @@ prj x;;*)

 (* injected homogeneous list (IHL) type "o". 
     'e1 and 'e2 for ground and logic type of the list element.
     g and l are abbreviations for notational convenience. *)

  type 'e g = ('e, 'b) t as 'b;;
  type 'e l = ('e, 'b) t logic as 'b;;
  type ('e1, 'e2) o = ('e1 g, 'e2 l) injected;;
  
  
  (* type management for IHL *)
  
  let nil () : ('e1, 'e2) o = Obj.magic Nil ;;
  let cons : ('e1, 'e2) injected ->  ('e1, 'e2) o -> ('e1, 'e2) o
    = fun x y -> Obj.magic @@ Cons(x,y) ;;

  (* Cons(x, y) : (('e1, 'e2) injected, ('e1, 'e2) o) t
                = (('e1, 'e2) injected, ('e1 g, 'e2 l) injected) t  (T0)

     distribution shall equate (T0) with type
     (('e1, 'e1 g) t, ('e2, 'e2 l) t logic) injected                (T1)

     note that 'e2 l = ('e2, 'b) t logic as 'b
                     = ('e2, ('e2, 'b) t logic as 'b) t logic
                     = ('e2, 'e2 l) t logic
     and       'e1 g = ('e1, 'b) t as 'b
                     = ('e1, ('e1, 'b) t as 'b) t
                     = ('e1, 'e1 g) t
    
     (T1) then equals to 
     ('e1 g, 'e2 l) injected = ('e1, 'e2) o 
   *)
    
end;;



(* (int, int) AList.t *)

Logic.fmap AList.prj @@
  AList.reify my_env
    (reify my_env)
    (reify my_env)
    ((Obj.magic (AList.Cons (Obj.magic my_var,
                             Obj.magic my_var)))
     : ((int, (int, AList.b) app) app,
        (int logic, (int logic, AList.b) app) app logic) injected);;

Logic.fmap AList.prj @@
  AList.reify my_env
    (reify my_env)
    (reify my_env)
    ((Obj.magic (AList.Nil))
     : ((int, (int, AList.b) app) app,
        (int logic, (int logic, AList.b) app) app logic) injected);;

(* (int foo, bool bar) AList.t *)

let tconv = fun x ->
  Logic.fmap AList.prj @@ Logic.fmap (AList.fmap (Logic.fmap Foo.prj) (Logic.fmap Bar.prj)) x
    in (tconv @@ AList.reify my_env
                   (Foo.reify my_env (reify my_env))
                   (Bar.reify my_env (reify my_env))
        @@ Obj.magic @@ AList.Cons(Obj.magic @@ Foo.Bar(Obj.magic 111, Obj.magic 222),
                                   Obj.magic @@ Bar.Baba(Obj.magic true,
                                                         Obj.magic my_var,
                                                         Obj.magic false))
                        : (int logic Foo.foo logic, bool logic Bar.bar logic) AList.t logic);;


(* (int, 'b) AList.t as 'b *)

(* The non-recursive type of this expr is (int, (int, ('a, ('b, ('c, 'd) t) t) t) t) t 
   which alludes to heterogeneous lists. *)

(* AList.inj is too shallow *)

AList.inj (AList.Cons(1,
           AList.Cons(2,
                      AList.Cons(Obj.magic my_var,
                                 AList.Cons(Obj.magic my_var,
                                            AList.Nil)))) : ((int, 'b) AList.t as 'b));;

let my_list =
  AList.cons
    ((Obj.magic 1) : (int, int logic) injected)
    (AList.cons
       ((Obj.magic 2) : (int, int logic) injected)
       (AList.cons
          ((Obj.magic my_var) : (int, int logic) injected)
          (AList.cons
             ((Obj.magic my_var) : (int, int logic) injected)
             (AList.nil ()))));;

(*
   This reifier "list_reify" does not type check because we cannot have types of the form 
   ('a, 'b) app as 'b for an abstract type app. 

   let rec list_reify x = AList.reify my_env (reify my_env) list_reify x;;

   Error: x has type ('a, 'b) injected
       but an expression was expected of type
         (('c, ('a, AList.b) app) app,
          ('c Logic.logic, ('b, AList.b) app) app Logic.logic)
         injected
         The type variable 'a occurs inside ('c, ('a, AList.b) app) app

   Comapre:

   Case 1:
   type ('a, 'b) t = Nil | Cons of 'a * 'b;;
   ('a, 'b) t as 'b        ----- alloed

   Case 2:
   type ('a, 'b) app
   ('a, 'b) app as 'b      ----- not allowed

   It might be helpful to provide app a representation or equation, which would allow Case 2.
 *)                   

let rec list_reify x = AList.reify my_env (reify my_env) list_reify x in
    ((Obj.magic (list_reify (Obj.magic my_list))) : int logic AList.l) ;;
