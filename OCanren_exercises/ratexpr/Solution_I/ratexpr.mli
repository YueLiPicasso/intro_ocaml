open Logic;;
open Core;;

(** Provide an alias for the name from the module [Logic] *)
@type 'a logic' = 'a logic
 with  show, html, eq, compare, foldl, foldr, gmap, fmt;;

(** Abstract type for arithmetic expressions of rational numbers *)
@type ('nat, 'self) rat_expr =
     Num of 'nat * 'nat              (** A rational number *)
   | Sum of 'self * 'self            (** Sum of rat expr *)
   | Subt of 'self * 'self           (** Subtraction of rat expr *)
   | Prod of 'self * 'self           (** Product of rat expr *)
 with show, html, eq, compare, foldl, foldr, gmap, fmt;;

(** Alias of the main type *)
@type ('a,'b) t = ('a,'b) rat_expr
 with show, html, eq, compare, foldl, foldr, gmap, fmt;;

(** Main type on [ground] level, unsigned and Peano *)
@type ground = (LNat.ground, ground) t
 with show, html, eq, compare, foldl, foldr, gmap, fmt;;

(** Main type on [logic] level *)
@type logic = (LNat.logic, logic) t logic'
 with  show, html, eq, compare, foldl, foldr, gmap, fmt;;

(** Main type on [injected] level *)
type groundi = (ground, logic) injected;;

(**  signed rational number, {e f} for {e full} *)
@type frat = (GT.int, frat) t
 with  show, html, eq, compare, foldl, foldr, gmap, fmt;;

(** Produce injected value using injected constructor arguments; reification *)
module Inj : sig
  val num  : LNat.groundi -> LNat.groundi -> groundi;; 
  val sum  : groundi -> groundi -> groundi;;
  val subt : groundi -> groundi -> groundi;;
  val prod : groundi -> groundi -> groundi;;
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

  (** It can simplify or complicate rational numbers. Forwad use is acceptable but
      less efficient than [simplify], and backward use is also acceptable *)
  val simplify : LNat.groundi -> LNat.groundi -> LNat.groundi -> LNat.groundi -> goal;;
  
  (** relational evaluator
      - Forward: OK.
      - Backward: OK.
      - Quine: OK. *)
  val eval : groundi -> groundi -> goal;;

  (** bounded search space, generate and test for both directions *)
  val evalb : groundi -> groundi -> goal;;

  module Prj : sig
    open LNat;;
    (** Similar to LoNat.Prj.logic_to_ground *)
    val logic_to_ground : logic * logic -> ground * ground;;
  end;;
end 
