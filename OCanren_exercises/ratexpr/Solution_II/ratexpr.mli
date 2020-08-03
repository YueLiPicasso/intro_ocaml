open Logic;;
open Core;;

(** Provide an alias for the name from the module [Logic] *)
@type 'a logic' = 'a logic
 with  show, html, eq, compare, foldl, foldr, gmap, fmt;;

(** Abstract type for arithmetic expressions of rational numbers. 
    This polymorphic variant setup allows specification at various
    levels. For instance, for the constructor [Num], its argument is: 
    {ol {- on [ground level]: a ground pair of ground nats;}
    {- on [logic level]: a logic pair of logic nats;}
    {- on [injected/groundi level]: an injected pair of injected nats.} } *)
@type ('a, 'b) rat_expr =
     Num  of 'a           (** A rational number *)
   | Sum  of 'b           (** Sum between rat expr *)
   | Subt of 'b           (** Subtraction between rat expr *)
   | Prod of 'b           (** Product between rat expr *)
 with show, html, eq, compare, foldl, foldr, gmap, fmt;;

(** Alias of the main type *)
@type ('a,'b) t = ('a,'b) rat_expr
 with show, html, eq, compare, foldl, foldr, gmap, fmt;;

(** Main type on [ground] level, unsigned and Peano *)
@type ground = ((LNat.ground, LNat.ground) LPair.ground, (ground, ground) LPair.ground) t
 with show, html, eq, compare, foldl, foldr, gmap, fmt;;

(** Main type on [logic] level *)
@type logic = ((LNat.logic, LNat.logic) LPair.logic,  (logic, logic) LPair.logic) t logic'
 with  show, html, eq, compare, foldl, foldr, gmap, fmt;;

(** Main type on [injected] level *)
type groundi = (ground, logic) injected;;

(** TYPe OPeratons: injection and reification *)
module Typop : sig
  val num  : (LNat.ground, LNat.logic, LNat.ground, LNat.logic) LPair.groundi -> groundi;; 
  val sum  : (ground, logic, ground, logic) LPair.groundi -> groundi;;
  val subt : (ground, logic, ground, logic) LPair.groundi -> groundi;;
  val prod : (ground, logic, ground, logic) LPair.groundi -> groundi;;
  val reify : VarEnv.t -> groundi -> logic;;
end;;

(** Some relations on LNat.groundi *)
module LoNat : sig
  open LNat;;

  module Prj : sig
    (** Attempt to covert data from LNat.logic to LNat.ground. Raise 
        Not_a_value exception if there is a free logic variable. *)
    val logic_to_ground : logic -> ground;;
  end;;
  
  (**  [divisible_by a b] holds if [a] is divisible by [b].
     This relation can: 
     {ol {- find all divisors of [a], or} 
     {- enumerate all multiples of [b], or} 
     {- enumerate all pairs [(a,b)] where a is divisible by b}} *)
  val divisible_by : groundi -> groundi -> goal;;

  (** [remainder a b r] if [r] is the remainder when [a] is divided by [b] *)
  val remainder    : groundi -> groundi -> groundi -> goal;;

  (** [gcd a b c] if the greatest common divisor of [a] and [b] is [c], 
      where [b <= a] *)
  val gcd          : groundi -> groundi -> groundi -> goal;;
end;;

(** Some relations on injected rational numbers *)
module LoRat : sig
  (** [simplify a b a' b'] simplifies a/b to the normal form a'/b'. 
    It can also be used to find all a/b that simplifies to a'/b'  *)
  val simplify :
    LNat.groundi -> LNat.groundi -> LNat.groundi -> LNat.groundi -> goal;;
(*
  val eval : groundi -> groundi -> goal;;*)
  
  module Prj : sig
    open LNat;;
    (** Similar to LoNat.Prj.logic_to_ground *)
    val logic_to_ground : (logic, logic) LPair.logic -> (ground, ground) LPair.ground;;
  end;;
end;; 

