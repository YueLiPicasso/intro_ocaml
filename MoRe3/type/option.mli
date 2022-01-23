open More
open More.Core   
    
type 'a t = 'a option
    
type 'a logic = 'a t Core.logic
    
type 'a ilogic = 'a t Core.ilogic

val none : unit-> 'a ilogic

val some : 'a -> 'a ilogic

val fmap : ('a -> 'b) -> 'a t -> 'b t
    
val reify : ('a, 'b) Reifier.t -> ('a ilogic, 'b logic) Reifier.t


(* Finitely nested options. 
   Although infinite nesting is possible under this type,
   for termination of the computation, use Seq instead. *)
module Rec : sig

  type logic  = 'b t Core.logic  as 'b
    
  type ilogic = 'b t Core.ilogic as 'b
    
  val reify : (ilogic,logic) Reifier.t 

end

(* lazily infinitely nested options *)
module Seq : sig

  type logic  = unit -> 'b t Core.logic  as 'b
    
  type ilogic = unit -> 'b t Core.ilogic as 'b
  
  val oo : ilogic
    
  val reify : (ilogic,logic) Reifier.t 

end


(* e.g., take 2 from SomeSomeSomeSome... we get SomeSomeNone. *)
val take : int -> Seq.logic -> Rec.logic


