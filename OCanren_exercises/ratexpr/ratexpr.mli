open Logic;;
open Core;;

(** Provide an alias for the name from the module Logic *)
@type 'a logic' = 'a logic with show, html, eq, compare, foldl, foldr, gmap, fmt;;

(** Type for arithmetical expressions 
   of positive rational numbers *)
@type ('nat, 'self) rat_expr =
     Num of 'nat * 'nat              (* A positive rational number *)
   | Sum of 'self * 'self            (* Sum of two rat expr *)
   | Subt of 'self * 'self           (* subtraction between two rat expr *)
   | Prod of 'self * 'self           (* Product of two rat expr *)
 with show, html, eq, compare, foldl, foldr, gmap, fmt;;

@type ('a,'b) t = ('a,'b) rat_expr with  show, html, eq, compare, foldl, foldr, gmap, fmt;;

@type ground = (LNat.ground, ground) t with  show, html, eq, compare, foldl, foldr, gmap, fmt;;

@type logic = (LNat.logic, logic) t logic' with  show, html, eq, compare, foldl, foldr, gmap, fmt;;

type groundi = (ground, logic) injected;;

(** Produce injected value using injected constructor arguments *)

module Inj : sig
  val num  : LNat.groundi * LNat.groundi -> groundi;;
  val sum  : groundi * groundi -> groundi;;
  val subt : groundi * groundi -> groundi;;
  val prod : groundi * groundi -> groundi;;
end;;

(** Operations on LNat.ground and ground rational *)
module GOOps : sig
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

  (* simplfy the ratio a/b by dividing with gcd *)
  val simplify : LNat.ground * LNat.ground -> LNat.ground * LNat.ground;;
  
end;;

(** Evaluate a rat expression to the simplified ratio form *)
(* val eval : ground -> ground;;*)
