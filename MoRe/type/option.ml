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

  let rec reify = lazy (Reifier.compose Reifier.reify 
      (Env.Lazy.bind reify (fun r ->
           Env.return (fun x ->
               match x with
               | Var _ as v' -> v'
               | Value t -> Value (fmap r t)))))

  (* The type intricacies of [reify] is detailed in [TA.reify] *)
    
  module TA = struct
   
    let rec reify =
      ((lazy
         (((((Reifier.compose :
                (ilogic, ilogic t Core.logic) Reifier.t
              -> (ilogic t Core.logic, logic) Reifier.t
              -> (ilogic, logic) Reifier.t)
               (Reifier.reify :
                  (ilogic, ilogic t Core.logic) Reifier.t))
            : (ilogic t Core.logic, logic) Reifier.t -> (ilogic, logic) Reifier.t)
             (((((Env.Lazy.bind
                  : (ilogic, logic) Reifier.t Lazy.t
                  -> ((ilogic ->  logic) -> (ilogic t Core.logic, logic) Reifier.t)
                  -> (ilogic t Core.logic, logic) Reifier.t)
                   (reify : (ilogic, logic) Reifier.t Lazy.t))
                : ((ilogic ->  logic) -> (ilogic t Core.logic, logic) Reifier.t)
                -> (ilogic t Core.logic, logic) Reifier.t)
                 ((fun (r : ilogic ->  logic) ->
                     (((Env.return
                        : (ilogic t Core.logic -> logic)
                        -> (ilogic t Core.logic, logic) Reifier.t)
                         ((fun (x : ilogic t Core.logic) ->
                             match x with
                             | Var _ as v' -> (v' : logic)
                             | Value (t : ilogic t) ->
                               ((Value ((fmap (r : ilogic -> logic) (t : ilogic t)) : logic t))
                                : logic))
                          : ilogic t Core.logic -> logic))
                      : (ilogic t Core.logic, logic) Reifier.t))
                  : (ilogic ->  logic) -> (ilogic t Core.logic, logic) Reifier.t))
              : (ilogic t Core.logic, logic) Reifier.t))
          : (ilogic, logic) Reifier.t))
       : (ilogic, logic) Reifier.t Lazy.t)
  end    
end





      

