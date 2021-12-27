open Eucpp
open Eucpp.Core

type 'a t = 'a option
    
type 'a logic = 'a t Core.logic
    
type 'a ilogic = 'a t Core.ilogic

let none = fun () -> inj None

let some = fun v -> inj (Some v)

let rec somes () = inj (Some (somes()))

let fmap = fun f -> function
  | Some a -> Some (f a)
  | None -> None

(* with explicit type annotation *)
let reify = fun (ra : ('a ->'b) Env.t) ->
  (((Env.bind :
       ('c Core.ilogic -> 'c Core.logic) Env.t
     -> (('c Core.ilogic -> 'c Core.logic) -> ('a ilogic -> 'b logic) Env.t)
     -> ('a ilogic -> 'b logic) Env.t)
      (Reifier.reify : ('c Core.ilogic -> 'c Core.logic) Env.t ))
   : (('c Core.ilogic -> 'c Core.logic) -> ('a ilogic -> 'b logic) Env.t)
   -> ('a ilogic -> 'b logic) Env.t)
    ((fun (r : 'c Core.ilogic -> 'c Core.logic) ->
        ((((((Env.bind : ('a ->'b) Env.t -> (('a ->'b) -> ('a ilogic -> 'b logic) Env.t)
              -> ('a ilogic -> 'b logic) Env.t)
               (ra : ('a ->'b) Env.t))
            : (('a ->'b) -> ('a ilogic -> 'b logic) Env.t) -> ('a ilogic -> 'b logic) Env.t)
             ((fun (fa : 'a -> 'b) ->
                 ((Env.return
                     ((fun (x : 'a t Core.ilogic) ->
                         match (r x : 'a t Core.logic) with
                         | Var _  as v' -> (v' : 'b t Core.logic)   (* Polymorphic Var *)
                         | Value t -> ((Value ((fmap (fa : 'a -> 'b) (t : 'a t)) : 'b t))
                                       : 'b t Core.logic)
                       ) : 'a ilogic -> 'b logic)
                  ) : ('a ilogic -> 'b logic) Env.t)
               ) : ('a ->'b) -> ('a ilogic -> 'b logic) Env.t )
          )) : ('a ilogic -> 'b logic) Env.t)
      ) :  ('c Core.ilogic -> 'c Core.logic) -> ('a ilogic -> 'b logic) Env.t)
    
(* Omitting type annotation

 let reify = fun ra ->
    let (>>=) = Env.bind in
    (Reifier.reify >>= (fun r -> (ra >>= (fun fa ->
      Env.return (fun x ->
        match r x with
        | Var _ as v'   -> v'
        | Value t -> Value (fmap fa t)
      )))))
*)

(* 
ra: reifier for the argument type 'a of 'a option
env: variable environment 

Option.reify ra env = fun x ->
        match (Reifier.reify env) x with
        | Var _ as v'  -> v'
        | Value t -> Value (Option.fmap (ra env) t)   
*)


let rec reify_inf () = Reifier.compose Reifier.reify 
      (Env.bind (reify_inf()) (fun r ->
           Env.return (fun x ->
               match x with
               | Var _ as v' -> v'
               | Value t -> Value (fmap r t))))
      
(*
reify_inf () env = fun x -> match (Reifier.reify env x) with
             | Var _ as v' -> v'
             | Value t -> Value (fmap (reify_inf () env) t) 
*)
  
