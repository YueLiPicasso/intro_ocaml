open GT;;
open OCanren;;
open OCanren.Std;;

(* To label the four vessels *)

@type vessel = A | B | C | D with show;;

(* injection primitives *)

module Vessel =struct
  let ves_a = !!A and ves_b = !!B and ves_c = !!C and ves_d = !!D;;
end;;

(* state of the vessels *)

@type state = int * int * int * int with show;;

(* volumns of each vessel *)

let volumn ves (vol : Nat.groundi) =
  let open Vessel in
  ocanren {
    ves == ves_a & vol == 24
  | ves == ves_b & vol == 13
  | ves == ves_c & vol == 11
  | ves == ves_d & vol == 5 
  };;

(* To query how much balsam a vessel contains in a given state *)

let how_much state ves (vol : Nat.groundi) =
  let open Vessel in
  ocanren {
    fresh a, b, c, d in state == (a,b,c,d) &
    { ves == ves_a & vol == a
    | ves == ves_b & vol == b
    | ves == ves_c & vol == c
    | ves == ves_d & vol == d }
};;

(* To set the content of a specified vessel and update the state *)

let refresh_state ves bal pre_state post_state =
  let open Vessel in
  ocanren {
    fresh a, b, c, d in
    pre_state == (a, b, c, d) &
    { ves == ves_a & post_state == (bal, b, c, d) |
      ves == ves_b & post_state == (a, bal, c, d) |
      ves == ves_c & post_state == (a, b, bal, d) |
      ves == ves_d & post_state == (a, b, c, bal) }
};;



let transfer_balsam from_ves to_ves from_state to_state =
  ocanren {
    fresh val_from_ves , (* balsam in from_ves before transfer *)
          val_from_ves', (* balsam in from_ves after transfer *)
          val_to_ves   , (* balsam in to_ves before transfer *)
          val_to_ves'  , (* balsam in to_ves after transfer *)
          volm_to_ves  , (* volumn of to_ves *)
          free_to_ves    (* empty space in to_ves *)
    in how_much from_state from_ves val_from_ves &
       how_much from_state to_ves   val_to_ves   &
        volumn to_ves volm_to_ves                &
       (+) free_to_ves val_to_ves volm_to_ves    & 
       { val_from_ves <= free_to_ves & }
  };;



(*

let vessel_A = ocanren { 24 }
and vessel_B = ocanren { 13 }
and vessel_C = ocanren { 11 }
and vessel_D = ocanren { 5  };;



(* possible moves to transfer balsam from one vessel to another *)

@type move = 
     From_A_to_B
   | From_A_to_C
   | From_A_to_D
   | From_B_to_A
   | From_B_to_C
   | From_B_to_D
   | From_C_to_A
   | From_C_to_B
   | From_C_to_D
   | From_D_to_A
   | From_D_to_B
   | From_D_to_C 
 with show;;

(* injection primitives *)

let from_A_to_B = !!From_A_to_B
and from_A_to_C = !!From_A_to_C
and from_A_to_D = !!From_A_to_D
and from_B_to_A = !!From_B_to_A
and from_B_to_C = !!From_B_to_C
and from_B_to_D = !!From_B_to_D
and from_C_to_A = !!From_C_to_A
and from_C_to_B = !!From_C_to_B
and from_C_to_D = !!From_C_to_D
and from_D_to_A = !!From_D_to_A
and from_D_to_B = !!From_D_to_B
and from_D_to_C = !!From_D_to_C
;;

    
(* valid state of the vessels: basic constraints *)

let valid_vessels_state a b c d =
  let open Nat in
  ocanren {
   a >= zero  & a <= !(vessel_A) &
   b >= zero  & b <= !(vessel_B) &
   c >= zero  & c <= !(vessel_C) &
   d >= zero  & d <= !(vessel_D) &
     fresh ab, abc, abcd in
       (+) a   b ab   &
       (+) ab  c abc  &
       (+) abc d abcd &
       abcd <= !(vessel_A)
  };; 


(* valid single step *)

let step move vessels vessels' =
  let open Nat in 
  ocanren {
    fresh a , b , c , d  in vessels  == (a , b , c , d ) &
    fresh a', b', c', d' in vessels' == (a', b', c', d') &
    { move == from_A_to_B &
      fresh b_free in
      (+) b b_free vessel_B &
      {  a <= b_free & a' == zero      & (+) b a b'     & c == c' & d == d'
       | a > b_free  & (+) a' b_free a & b' == vessel_B & c == c' & d == d'} 
    }
  };;


*)
