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
  open LNat;;
  
  (** equallity *)
  val ( = )  : ground -> ground -> GT.bool;;
  
  (** less than *)
  val ( < )  : ground -> ground -> GT.bool;;

  (** less than or equal *)
  val ( <= ) : ground -> ground -> GT.bool;;

  (** Addition *)
  val ( + )  : ground -> ground -> ground;;
  
  (** Subtraction. 
      Subtracting a number greater than self is forbidden. *)
  val ( - )  : ground -> ground -> ground;;

  (** Multiplication *)
  val ( * )  : ground -> ground -> ground;;

  (** Division returns the (quotient, remainder) pair 
      with protection against division by zero *)
  val ( / )  : ground -> ground -> ground * ground;;

  (** greatest common divisor *)
  val gcd    : ground -> ground -> ground;;

  (** Convert to normal form a rational number a/b represented as (a,b) *)
  val simplify : ground * ground -> ground * ground;;
  
end;;

(** Operations on ground rational numbers *)
module GRat : sig
  (** Convert between, say, [Num (0,1)] and [Num (O, S O)] *)
  val to_frat : ground -> frat;;
  val of_frat : frat -> ground;; (** Sign ignored *)

  (** Evaluate an expression to normal form *)
  val eval : ground -> ground;;
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
     This relation can: {ol {- find all divisors of [a], or} 
     {- enumerate all multiples of [b], or} 
     {- enumerate all pairs [(a,b)] where a is divisible by b}} *)
  val divisible_by : groundi -> groundi -> goal;;
  (** [remainder a b r] if [r] is the remainder when [a] is divided by [b] *)
  val remainder    : groundi -> groundi -> groundi -> goal;;
  (** [div a b q r] if [a = b * q + r]. Use like [div ! ! ? ?] 
      or [div ? ? ! !] or any other combination of [!] and [?]. *)
  val div          : groundi -> groundi -> groundi -> groundi -> goal;;
  module NonCommutative : sig
    (** [gcd a b c] if the greatest common divisor of [a] and [b] (where [a >= b]) is [c] *)
    val gcd          : groundi -> groundi -> groundi -> goal;;
    (** [lcm a b c] if the leatest common multiple of [a] and [b] (where [a >= b]) is [c] *)
    val lcm          : groundi -> groundi -> groundi -> goal;;
    (** compute within a finite space where both the  numerator and the 
        denomiator range over an implementation-dependent finite set. *)
    module Bounded : sig
      val gcd       : groundi -> groundi -> groundi -> goal;;
      val lcm       : groundi -> groundi -> groundi -> goal;;
    end;;
  end;;
  (** [gcd a b c] if the greatest common divisor of [a] and [b] is [c] *)
  val gcd          : groundi -> groundi -> groundi -> goal;;
  (** [lcm a b c] if the leatest common multiple of [a] and [b] is [c] *)
  val lcm          : groundi -> groundi -> groundi -> goal;;
  (** [simplify a b c d] if the normal form of [a/b] is [c/d] *)
  val simplify     : groundi -> groundi -> groundi -> groundi -> goal;;
  (** Similar to [simplify], optimized for forward run when [a] and [b] are concrete *)
  val simplify_f   : groundi -> groundi -> groundi -> groundi -> goal;;
  (** [radd_core a b a' b' c d] if  [b == b] and [a/b + a'/b'] has normal form [c/d] *)
  val radd_core    : groundi -> groundi -> groundi -> groundi -> groundi -> groundi-> goal;;
  (** similar to [radd_core] but is optimized for forward run when [a,b,a',b'] are concrete *)
  val radd_core_f    : groundi -> groundi -> groundi -> groundi -> groundi -> groundi-> goal;;
  module NonCom : sig
    val radd : groundi -> groundi -> groundi -> groundi -> groundi -> groundi-> goal;;
    module Bounded : sig
      val radd : groundi -> groundi -> groundi -> groundi -> groundi -> groundi-> goal;;
    end;;
  end;;
end;;
(*
(** Some relations on injected rational numbers *)
module LoRat : sig

  (** relational evaluator *)
  val eval : groundi -> groundi -> goal;;

  (** relational evaluator with bounded search space *)
  val evalb : groundi -> groundi -> goal;;

end;; *) 
