(*
There are floating-point intervals, string intervals, time
intervals and so on; For each we have similar operations: testing for 
emptiness, checking for containment, intersecting intervals, etc.

We use functors to build an interval library that can be used for any 
type where an interval makes sense. This requires that the (underlying
set of the) type supports a total order. 
*)


module type Comparable = sig
  type t;;
  val compare : t -> t -> int;;
end;;

module Interval (Endpoint : Comparable) = struct
  type t = Interval of Endpoint.t * Endpoint.t
         | Empty;;

  let create : Endpoint.t -> Endpoint.t -> t =
    fun low high ->
    if Endpoint.compare low high > 0 then Empty
    else Interval (low, high);;

  let is_empty : t -> bool = function
      Empty -> true
    | _ -> false;;

  let contains : t -> Endpoint.t -> bool = fun t x ->
    match t with
      Empty -> false
    | Interval (l, h) ->
       Endpoint.compare x l >= 0 && Endpoint.compare x h <= 0;;

let intersect : t -> t -> t = fun t1 t2  ->
  let min x y = if Endpoint.compare x y <= 0 then x else y in
  let max x y = if Endpoint.compare x y >= 0 then x else y in
  match t1,t2 with
  | Empty, _ | _, Empty -> Empty
  | Interval (l1, h1) , Interval (l2, h2) ->
     create (max l1 l2) (min h1 h2);;

end;;

module Int_interval =
  Interval(struct
      type t = int
      let compare = Int.compare
    end);;

module Intm = struct
  type t = int
  let compare = Int.compare
end;;

module Int_interval' =
  Interval(Intm);;

(* compare the signatures of Int_interval and Int_interval' *)

Int_interval'.create 4 8;;

(* It is known to the type checker that Intm.t = int but the functor body 
of Interval does not know it *)

module Int_interval'' = Interval(Int);;
module String_interval = Interval(String);;

Int_interval''.intersect
(Int_interval''.create 3 6)
(Int_interval''.create 4 8);;
