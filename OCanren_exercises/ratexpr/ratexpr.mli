open Logic;;
open Core;;

(** Provide an alias for the name from the module Logic *)
@type 'a logic' = 'a logic with show, html, eq, compare, foldl, foldr, gmap, fmt;;

(** Abstract type for arithmetical expressions 
   of positive rational numbers *)
@type ('nat, 'self) rat_expr =
     Num of 'nat * 'nat              (* A positive rational number *)
   | Sum of 'self * 'self            (* Sum of two rat expr *)
   | Subt of 'self * 'self           (* subtraction between two rat expr *)
   | Prod of 'self * 'self           (* Product of two rat expr *)
 with show, html, eq, compare, foldl, foldr, gmap, fmt;;

(** Alias of the main type *)
@type ('a,'b) t = ('a,'b) rat_expr with  show, html, eq, compare, foldl, foldr, gmap, fmt;;

(** Main type on [ground] level, unsigned and Peano *)
@type ground = (LNat.ground, ground) t with  show, html, eq, compare, foldl, foldr, gmap, fmt;;

(** Main type on [logic] level *)
@type logic = (LNat.logic, logic) t logic' with  show, html, eq, compare, foldl, foldr, gmap, fmt;;

(** Main type on [injected] level *)
type groundi = (ground, logic) injected;;

(** full signed rational number *)
@type frat = (GT.int, frat) t with  show, html, eq, compare, foldl, foldr, gmap, fmt;;

(** Produce injected value using injected constructor arguments *)
module Inj : sig
  val num  : LNat.groundi * LNat.groundi -> groundi;;
  val sum  : groundi * groundi -> groundi;;
  val subt : groundi * groundi -> groundi;;
  val prod : groundi * groundi -> groundi;;
end;;

(** Operations on LNat.ground *)
module GNat : sig
  (* equallity *)
  val ( = )  : LNat.ground -> LNat.ground -> GT.bool;;
  
  (* less than *)
  val ( < )  : LNat.ground -> LNat.ground -> GT.bool;;

  (* less than or equal *)
  val ( <= ) : LNat.ground -> LNat.ground -> GT.bool;;

  (* Addition *)
  val ( + )  : LNat.ground -> LNat.ground -> LNat.ground;;
  
  (* subtraction *)
  val ( - )  : LNat.ground -> LNat.ground -> LNat.ground;;

  (* Multiplication *)
  val ( * )  : LNat.ground -> LNat.ground -> LNat.ground;;

  (* division with (quotient, remainder) *)
  val ( / )  : LNat.ground -> LNat.ground -> LNat.ground * LNat.ground;;

  (* greatest common divisor *)
  val gcd    : LNat.ground -> LNat.ground -> LNat.ground;;
  
end;;

(** Operations on ground rational numbers *)
module GRat : sig
  (** Evaluate an expression to normal form *)
  val eval : ground -> ground;;

  (** Convert to normal form a rational number a/b represented as (a,b) *)
  val simplify : LNat.ground * LNat.ground -> LNat.ground * LNat.ground;;

  (** Convert between,  say, Num (0,1) and Num (O, S O) *)
  val to_int : ground -> frat;;
  val of_int : frat -> ground;; (* sign ignored *)
end;;
