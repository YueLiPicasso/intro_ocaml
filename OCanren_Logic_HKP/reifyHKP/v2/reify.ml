#rectypes;;

(*---------------------------------------------------------*)
(*--light-weight higher-kinded polymorphic type encoding---*)
(*---------------------------------------------------------*)

type ('a, 'b) app = App of 'a * 'b;;

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
              
module Logic = struct

  type ('a, 'b) injected;;
  
  type 'a logic = Var of int * 'a logic list | Value of 'a;;              

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
  
end;;

(*----------------------------------------------------*)
(*---------------------some testss--------------------*)
(*----------------------------------------------------*)

let my_env = VarEnv.empty ();;
let my_var = Term.Var.make ~env:12 ~scope:0 12345;;

module AList = struct
  
  type ('a, 'b) t = Nil | Cons of 'a * 'b;;

  type b;;

  external inj : ('a, 'c) t -> ('a, ('c, b) app) app = "%identity";;
    
  external prj : ('a, ('c, b) app) app -> ('a, 'c) t = "%identity" ;;                         
                                                       
  let fmap' : ('a -> 'b) -> ('c -> 'd) ->  ('a, 'c) t -> ('b, 'd) t =
      fun f g -> function Nil -> Nil | Cons(a,b) -> Cons(f a, g b);;
  
  let fmap : ('a -> 'b) -> ('c -> 'd) ->  ('a, ('c, b) app) app ->  ('b, ('d, b) app) app =
    fun f g x -> inj @@ fmap' f g @@ prj x;;
    
  let reify env subr1 subr2 x = Logic.reify2 ~env ~fmap ~subr1 ~subr2 x;;
    
end;;

module LList = struct

  open AList;;
  
  module App = struct
    type 'a g = ('a, ('b, b) app) app as 'b;;
    type 'a l = ('a, ('b, b) app) app Logic.logic as 'b;;
    type ('a, 'b) o = ('a g, 'b l) Logic.injected;;
    
    let rec reify : VarEnv.t -> (('a, 'b) Logic.injected -> 'b) -> ('a, 'b) o -> 'b l
      = fun env subr x -> AList.reify env subr (reify env subr) x;;
  end;;
  
  module Std = struct
    
    type 'a g = ('a, 'b) t as 'b;;
    type 'a l = ('a, 'b) t Logic.logic as 'b;;
    type ('a, 'b) o = ('a g, 'b l) Logic.injected;;
    
    let nil () : ('e1, 'e2) o = Obj.magic Nil ;;
    let cons : ('e1, 'e2) Logic.injected ->  ('e1, 'e2) o -> ('e1, 'e2) o
      = fun x y -> Obj.magic @@ Cons(x,y) ;;
    
    let reify : VarEnv.t -> (('a, 'b) Logic.injected -> 'b) ->
                ('a, 'b) o -> 'b l
      = fun env subr x -> Obj.magic @@ App.reify env subr (Obj.magic x);;
  end;;
  
end;;
    
  
let open LList.Std in
    let my_list = 
      cons
        ((Obj.magic 1) : (int, int Logic.logic) Logic.injected)
        (cons
           ((Obj.magic 2) : (int, int Logic.logic) Logic.injected)
           (cons
              ((Obj.magic my_var) : (int, int Logic.logic) Logic.injected)
              (cons
                 ((Obj.magic my_var) : (int, int Logic.logic) Logic.injected)
                 (nil ()))))
    in reify my_env (Logic.reify my_env) my_list;;
    

         
module ANat = struct
  
  type 'a t = Zero | Succ of 'a;;

  type b;;

  external inj : 'a t -> ('a, b) app = "%identity";;
    
  external prj : ('a, b) app -> 'a t = "%identity" ;;                         
                                                       
  let fmap' : ('a -> 'b) ->  'a t -> 'b t =
      fun f -> function Zero -> Zero | Succ n -> Succ (f n);;
  
  let fmap : ('a -> 'b) ->  ('a, b) app ->  ('b, b) app =
    fun f x -> inj @@ fmap' f @@ prj x;;
    
  let reify env subr x = Logic.reify1 ~env ~fmap ~subr x;;
  
end;;

module LNat = struct

  open ANat;;
  
  module App = struct
    type g = ('b, b) app as 'b;;
    type l = ('b, b) app Logic.logic as 'b;;
    type o = (g, l) Logic.injected;;
   
    let rec reify : VarEnv.t -> o -> l
      = fun env x -> ANat.reify env (reify env) x;;
  end;;
  
  module Std = struct
    
    type g = 'b t as 'b;;
    type l = 'b t Logic.logic as 'b;;
    type o = (g, l) Logic.injected;;
    
    let zero () : o = Obj.magic Zero ;;
    let succ : o -> o = fun x -> Obj.magic @@ Succ x ;;
    
    let reify : VarEnv.t -> o -> l
      = fun env x -> Obj.magic @@ App.reify env (Obj.magic x);;
  end;;
  
end;;


let open LNat.Std in
    let my_nat = succ (succ (succ (zero ()))) in reify my_env my_nat;;

let open LNat.Std in
    let my_nat =  succ (succ (succ (Obj.magic my_var)))
    in reify my_env my_nat;;

let open LNat.Std in
    let open LList.Std in
    let my_natlist = 
      let one = succ (zero()) in
      let two = succ one in
      let tri = succ two in
      let fur = succ tri in
      let eks = succ (Obj.magic my_var) in
      cons one (cons two (cons tri (cons eks (cons fur (nil ())))))
    in
    LList.Std.reify my_env (LNat.Std.reify my_env) my_natlist;;
    
