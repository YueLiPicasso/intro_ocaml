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


(*





(**  signed rational number, {e f} for {e full} *)
@type frat = (GT.int, frat) t
 with  show, html, eq, compare, foldl, foldr, gmap, fmt;;

(** Produce injected value using injected constructor arguments; reification *)
module Inj : sig
  val num  : LNat.groundi * LNat.groundi -> groundi;;
  val sum  : groundi * groundi -> groundi;;
  val subt : groundi * groundi -> groundi;;
  val prod : groundi * groundi -> groundi;;
  val reify : VarEnv.t -> groundi -> logic;;
end;;

(** Operations on [LNat.ground] *)
module GNat : sig
  (** equallity *)
  val ( = )  : LNat.ground -> LNat.ground -> GT.bool;;
  
  (** less than *)
  val ( < )  : LNat.ground -> LNat.ground -> GT.bool;;

  (** less than or equal *)
  val ( <= ) : LNat.ground -> LNat.ground -> GT.bool;;

  (** Addition *)
  val ( + )  : LNat.ground -> LNat.ground -> LNat.ground;;
  
  (** Subtraction. 
      Subtracting a number greater than self is forbidden. *)
  val ( - )  : LNat.ground -> LNat.ground -> LNat.ground;;

  (** Multiplication *)
  val ( * )  : LNat.ground -> LNat.ground -> LNat.ground;;

  (** Division returns the (quotient, remainder) pair 
      with protection against division by zero *)
  val ( / )  : LNat.ground -> LNat.ground -> LNat.ground * LNat.ground;;

  (** greatest common divisor *)
  val gcd    : LNat.ground -> LNat.ground -> LNat.ground;;
  
end;;

(** Operations on ground rational numbers *)
module GRat : sig
  (** Evaluate an expression to normal form *)
  val eval : ground -> ground;;

  (** Convert to normal form a rational number a/b represented as (a,b) *)
  val simplify : LNat.ground * LNat.ground -> LNat.ground * LNat.ground;;

  (** Convert between, say, [Num (0,1)] and [Num (O, S O)] *)
  val to_frat : ground -> frat;;
  val of_frat : frat -> ground;; (** Sign ignored *)
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
  val simplify : LNat.groundi -> LNat.groundi -> LNat.groundi -> LNat.groundi -> goal;;

  val eval : groundi -> groundi -> goal;;
  
  module Prj : sig
    open LNat;;
    (** Similar to LoNat.Prj.logic_to_ground *)
    val logic_to_ground : logic * logic -> ground * ground;;
  end;;
end 
*)
