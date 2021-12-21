open Eucpp
open Eucpp.Core

type 'a t = 'a option
    
type 'a logic = 'a t Core.logic
    
type 'a ilogic = 'a t Core.ilogic

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
                         | Var v  as v' -> (v' : 'b t Core.logic)   (* Polymorphic Var *)
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
        | Var v   -> Var v
        | Value t -> Value (fmap fa t)
      )))))
*)

(* 
ra: reifier for the argument type 'a of 'a option
env: variable environment 

Option.reify ra env = fun x ->
        match (Reifier.reify env) x with
        | Var v   -> Var v
        | Value t -> Value (Option.fmap (ra env) t)   
*)
