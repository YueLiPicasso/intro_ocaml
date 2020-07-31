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

(** Produce injected value using injected constructor arguments *)
module Inj : sig
  val num  : LNat.groundi * LNat.groundi -> groundi;;
  val sum  : groundi * groundi -> groundi;;
  val subt : groundi * groundi -> groundi;;
  val prod : groundi * groundi -> groundi;;
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

(**  [divisible_by a b] holds if [a] is divisible by [b].
     This relation can: 
     {ol {- find all divisors of [a], or} 
     {- enumerate all multiples of [b], or} 
     {- enumerate all pairs [(a,b)] where a is divisible by b}} *)
val divisible_by : LNat.groundi -> LNat.groundi -> goal;;

(** [common_divisor a b c] holds if [c] is a common divisor of [a] and [b]. 
    {ol {- Given }}*)
val common_divisor :  LNat.groundi -> LNat.groundi -> LNat.groundi -> goal;;

  
(** [simplify a b a' b'] simplifies a/b to the normal form a'/b' and fails if b is zero. *)
val simplify : LNat.groundi -> LNat.groundi -> LNat.groundi -> LNat.groundi -> goal;;

