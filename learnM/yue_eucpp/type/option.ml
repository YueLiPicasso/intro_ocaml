open Eucpp
open Eucpp.Core

type 'a t = 'a option
    
type 'a logic = 'a t Core.logic
    
type 'a ilogic = 'a t Core.ilogic

let fmap = fun f -> function
  | Some a -> Some (f a)
  | None -> None

let reify = fun (ra : ('a ->'b) Env.t) ->
  (Env.bind :
     ('a2 Core.ilogic -> 'a2 Core.logic) Env.t
   -> (('a2 Core.ilogic -> 'a2 Core.logic) -> 'b3 Env.t)
   -> 'b3 Env.t)
    (Reifier.reify : ('a2 Core.ilogic -> 'a2 Core.logic) Env.t )
    (fun r ->
       ((Env.bind : ('a ->'b) Env.t -> (('a ->'b) -> 'b1 Env.t) -> 'b1 Env.t)
          (ra : ('a ->'b) Env.t)
          ((fun (fa : 'a -> 'b) ->
             ((Env.return
               ((fun x ->
                  match r x with
                  | Var v as v' -> v'
                  | Value t -> Value (fmap fa t)
                ) : 'b1)
              ) : 'b1 Env.t)
            ) : ('a ->'b) -> 'b1 Env.t )
       )
    )
