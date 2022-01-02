open More
open More.Core

type 'a t = 'a option
    
type 'a logic = 'a t Core.logic
    
type 'a ilogic = 'a t Core.ilogic

let none = fun () -> inj None

let some = fun v -> inj (Some v)

let fmap = fun f -> function
  | Some a -> Some (f a)
  | None -> None

let reify = fun ra ->
  let (>>=) = Env.bind in
  (Reifier.reify >>= (fun r -> (ra >>= (fun fa ->
       Env.return (fun x ->
           match r x with
           | Var _ as v'   -> v'
           | Value t -> Value (fmap fa t))))))
  
(* The type intricacies of [reify] is detailed in [TA.reify] *)

module TA = struct
 
  let reify =
    ((fun (ra : ('a,'b) Reifier.t) ->
        (((((Env.bind :
               ('c Core.ilogic, 'c Core.logic) Reifier.t
             -> (('c Core.ilogic -> 'c Core.logic) -> ('a ilogic, 'b logic) Reifier.t)
             -> ('a ilogic, 'b logic) Reifier.t)
              (Reifier.reify : ('c Core.ilogic, 'c Core.logic) Reifier.t ))
           : (('c Core.ilogic -> 'c Core.logic) -> ('a ilogic, 'b logic) Reifier.t)
           -> ('a ilogic, 'b logic) Reifier.t)
            ((fun (r : 'c Core.ilogic -> 'c Core.logic) ->
                ((((((Env.bind
                      : ('a, 'b) Reifier.t
                      -> (('a -> 'b) -> ('a ilogic, 'b logic) Reifier.t)
                      -> ('a ilogic, 'b logic) Reifier.t)
                       (ra : ('a, 'b) Reifier.t))
                    : (('a -> 'b) -> ('a ilogic, 'b logic) Reifier.t)
                    -> ('a ilogic, 'b logic) Reifier.t)
                     ((fun (fa : 'a -> 'b) ->
                         ((Env.return
                             ((fun (x : 'a ilogic) ->
                                 match (r x : 'a logic) with
                                 | Var _  as v' -> (v' : 'b logic)   (* Polymorphic Var *)
                                 | Value t ->
                                   ((Value ((fmap (fa : 'a -> 'b) (t : 'a t)) : 'b t))
                                    : 'b logic))
                              : 'a ilogic -> 'b logic))
                          : ('a ilogic, 'b logic) Reifier.t))
                      : ('a ->'b) -> ('a ilogic, 'b logic) Reifier.t )))
                 : ('a ilogic, 'b logic) Reifier.t))
             : ('c Core.ilogic -> 'c Core.logic) -> ('a ilogic, 'b logic) Reifier.t))
         : ('a ilogic, 'b logic) Reifier.t))
     : ('a,'b) Reifier.t -> ('a ilogic, 'b logic) Reifier.t)
end

module Nested = struct

  type nonrec logic =  'b logic as 'b
    
  type nonrec ilogic =  'b ilogic as 'b
    
  let rec some () = inj (Some (some()))

  let rec reify =
    let (>>=) = Env.bind
    and (>>>=) = Reifier.Lazy.bind in 
    lazy (Reifier.reify >>= (fun sr -> reify >>>= (fun rr -> Env.return (fun x ->
        match sr x with
        | Var _ as v -> v
        | Value t -> Value (fmap (Reifier.Lazy.force rr) t)))))
end





      

