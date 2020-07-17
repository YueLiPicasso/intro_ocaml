open GT;;
open OCanren;;
module L = Stdlib.List;;
open OCanren.Std;;

(* To label the four vessels *)

@type vessel = A | B | C | D with show;;

(* injection primitives *)

module Vessel =struct
  let ves_a = !!A and ves_b = !!B and ves_c = !!C and ves_d = !!D;;
end;;

(* state of the vessels *)

@type state = int * int * int * int with show;;

(* volumn of each vessel *)

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


let from_to ves_1 ves_2 =
  let open Vessel in
  ocanren { ves_1 == ves_a & ves_2 == ves_b |
            ves_1 == ves_a & ves_2 == ves_c |
            ves_1 == ves_a & ves_2 == ves_d |
            
            ves_1 == ves_b & ves_2 == ves_a |
            ves_1 == ves_b & ves_2 == ves_c |
            ves_1 == ves_b & ves_2 == ves_d |
            
            ves_1 == ves_c & ves_2 == ves_a |
            ves_1 == ves_c & ves_2 == ves_b |
            ves_1 == ves_c & ves_2 == ves_d |
            
            ves_1 == ves_d & ves_2 == ves_a |
            ves_1 == ves_d & ves_2 == ves_b |
            ves_1 == ves_d & ves_2 == ves_c 
          };;


let transfer_balsam from_ves to_ves from_state to_state =
  let open Nat in
  ocanren {
    fresh val_from_ves , (* balsam in from_ves before transfer *)
          val_from_ves', (* balsam in from_ves after transfer *)
          val_to_ves   , (* balsam in to_ves before transfer *)
          val_to_ves'  , (* balsam in to_ves after transfer *)
          volm_to_ves  , (* volumn of to_ves *)
          free_to_ves  , (* empty space in to_ves before transfer *)
          aux_state      (* helper variable *)
    in from_to from_ves to_ves                   &
       how_much from_state from_ves val_from_ves &
       val_from_ves > zero                       &
       how_much from_state to_ves val_to_ves     &
       volumn to_ves volm_to_ves                 &
       (+) free_to_ves val_to_ves volm_to_ves    &
       free_to_ves > zero                        &
       { val_from_ves <= free_to_ves
       & (+) val_from_ves val_to_ves val_to_ves'               
       & refresh_state from_ves zero        from_state aux_state      
       & refresh_state to_ves   val_to_ves' aux_state  to_state 
       |
         val_from_ves > free_to_ves                            
       & (+) free_to_ves val_from_ves' val_from_ves            
       & refresh_state from_ves val_from_ves' from_state aux_state
       & refresh_state to_ves   volm_to_ves   aux_state  to_state
       }
};;


module Steps = struct
  let f = Tabling.(tabledrec three) (* use tabling to avoid looping *)
      (fun f moves pre_state post_state ->
         ocanren {
           moves == [] & pre_state == post_state |
           fresh mid_state, m, ms, fves, tves in
               moves == m :: ms                 
             & m == (fves, tves) 
             & transfer_balsam fves tves pre_state mid_state         
             & f ms mid_state post_state  })
end;;

(* reification primitives *)

let prj_state x = match project x with (a,(b,(c,d))) -> (Nat.to_int a, Nat.to_int b, Nat.to_int c, Nat.to_int d )                                                        
and prj_moves x = List.to_list (fun (a,b) -> a,b) @@ project x;;

(* do some test next for the above relations *)

let print_str_nl s = print_string s; print_newline ();; 

let _ = 
L.iter print_str_nl @@ L.map (show(state)) @@ Stream.take @@
run q (fun q -> ocanren {fresh v1, v2 in transfer_balsam v1 v2 (24,0,0,0) q }) prj_state ;
print_newline();
L.iter print_str_nl @@ L.map (show(state)) @@ Stream.take @@
run q (fun q -> ocanren {fresh v1, v2 in transfer_balsam v1 v2 (11,13,0,0) q }) prj_state ;;

@type moves = (vessel * vessel) GT.list with show;;

let rec print_moves = 
  function [] -> ()
         | ms :: mss' ->
           print_str_nl (show(moves) ms);
           Printf.printf "Or\n";
           print_moves mss';;
                                    

let _  =
print_moves @@ Stream.take ~n:10000 @@ 
run q (fun q -> ocanren {Steps.f q (24,0,0,0) (8,8,8,0)}) prj_moves ;;
  

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
