open GT;;
open OCanren;;
module L = List;;
open OCanren.Std;;

module Vessel =struct

  @type vessel = A | B | C | D with show;; (* To label the four vessels *)
  @type move = vessel * vessel with show;;
  @type moves = move GT.list with show;;
  @type state = int * int * int * int with show;; (* state of the vessels *)
  @type states = state GT.list with show;;
  
  (* injection primitives *)
  
  let ves_a = !!A and ves_b = !!B and ves_c = !!C and ves_d = !!D;;
  
  let inj_ves = function A -> ves_a | B -> ves_b | C -> ves_c | D -> ves_d;;
  
  let inj_move (m : move) = match m with a,b -> ocanren { (inj_ves a, inj_ves b) };;

  let inj_moves (ms : moves) = List.list (L.map inj_move ms);;

  (* projection primitives *)

  let to_int_quads = fun (a,(b,(c,d))) -> (Nat.to_int a, Nat.to_int b, Nat.to_int c, Nat.to_int d );;
  
  let prj_state x = to_int_quads @@ project x;; 

  let prj_states x = List.to_list to_int_quads  @@ project x;;
  
  let prj_moves x = List.to_list (fun (a,b) -> a,b) @@ project x;;

  module Action = struct

    (* volumn of each vessel *)

    let volumn ves (vol : Nat.groundi) =
      ocanren {
        ves == ves_a & vol == 24
      | ves == ves_b & vol == 13
      | ves == ves_c & vol == 11
      | ves == ves_d & vol == 5 };;

    (* To query how much balsam a vessel contains in a given state *)

    let how_much state ves (vol : Nat.groundi) =
      ocanren {
        fresh a, b, c, d in state == (a,b,c,d) &
                            { ves == ves_a & vol == a
                            | ves == ves_b & vol == b
                            | ves == ves_c & vol == c
                            | ves == ves_d & vol == d }};;

    (* To set the content of a specified vessel and update the state *)

    let refresh_state ves bal pre_state post_state =
      ocanren {
        fresh a, b, c, d in
          pre_state == (a, b, c, d) &
        { ves == ves_a & post_state == (bal, b, c, d) |
          ves == ves_b & post_state == (a, bal, c, d) |
          ves == ves_c & post_state == (a, b, bal, d) |
          ves == ves_d & post_state == (a, b, c, bal) }};;

    (* To specify  valid directions of balsam transfer *)

    let from_to ves_1 ves_2 =
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
                ves_1 == ves_d & ves_2 == ves_c };;


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
            }};;
  end;;
end;;


module Steps = struct

  open Vessel;;
  open Vessel.Action;;

  (* To use as the solution finder *)
  
  let f =
    let f' = Tabling.(tabledrec four) 
        (fun f' last_mv moves pre_state post_state ->
           ocanren {
             moves == [] & pre_state == post_state |
             fresh mid_state, m, ms, fves, tves in
               moves == m :: ms
             & m =/= last_mv
             & m == (fves, tves)
             & last_mv =/= (tves, fves)
             & transfer_balsam fves tves pre_state mid_state 
             & f' m ms mid_state post_state  })
    in fun mvs fstat tstat -> ocanren { f' (ves_a, ves_a) mvs fstat tstat } ;;

   (* To use as the answer checker *)

   let rec g actions init_state all_states =
     ocanren {
            actions == [] & all_states == [init_state] | 
            fresh act, acts, fves, tves, next_state, tail_states in
              actions  == act :: acts
            & act == (fves, tves)
            & transfer_balsam fves tves init_state next_state
            & all_states == init_state :: tail_states
            & g acts next_state tail_states };;

  (* To wrap f to make a function *)

  let acceptable_moves k =
    Stream.take ~n:k @@ 
    run q (fun q -> ocanren {f q (24,0,0,0) (8,8,8,0)}) prj_moves ;;

  (* To wrap g to make a function *)

  let result_of_moves =
    fun ms ->  let imvs = inj_moves ms in
    L.hd  @@ Stream.take ~n:1 @@
    run q (fun q -> ocanren {g imvs (24,0,0,0) q }) prj_states ;;

  let correct_answers k = L.map result_of_moves @@ acceptable_moves k ;;

  let good_answers k =
    let  not_mem a l = not @@ L.mem a l in
    let rec distinct_states = function
      | [] -> true
      | h :: t ->  (not_mem h t) && (distinct_states t)
    in
    L.filter distinct_states @@ correct_answers k;;
    
end;;

module Test = struct

  open Vessel;;
  open Vessel.Action;;
  open Steps;;

  let print_str_nl s = print_string s; print_newline (); print_newline ();;

  let ans = L.map (show(states)) @@ good_answers 1000 in
  Printf.printf "%d interesting solutions are:\n" @@ L.length ans;
  L.iter print_str_nl ans;; 

  (* Extra tests
  (* test the basic state transition procedure *)
  
  let _ = 
    L.iter print_str_nl @@ L.map (show(state)) @@ Stream.take @@
    run q (fun q -> ocanren {fresh v1, v2 in transfer_balsam v1 v2 (24,0,0,0) q }) prj_state ;
    print_newline();
    L.iter print_str_nl @@ L.map (show(state)) @@ Stream.take @@
    run q (fun q -> ocanren {fresh v1, v2 in transfer_balsam v1 v2 (11,13,0,0) q }) prj_state ;;

  let rec print_moves = 
    function [] -> ()
          | ms :: mss' ->
            print_str_nl (show(moves) ms);
            print_newline ();
            print_moves mss';;

  (* test the solution finder: set ~n to larger numbers for more answers, but they may not be interesting because of redundant moves *)

  let _  =  print_moves @@ acceptable_moves 50 ;;

  (* test the answer checker: provide a sequence of moves found by Step.f *)

  let _  =
    print_str_nl @@ (show(states)) @@  
    result_of_moves [(A, D); (A, C); (D, A); (A, B); (B, D); (C, A); (D, A); (B, C); (A, B); (B, D); (D, A)] ;;

  let _  =
    print_str_nl @@ (show(states)) @@  
    result_of_moves [(A, B); (A, C); (C, D); (D, A); (B, C); (A, B); (C, A); (B, C); (A, B); (C, D); (C, A);
                     (B, A); (D, A); (A, B); (A, C); (B, D); (C, A); (D, A); (B, C); (A, B); (B, D); (D, A)];;

  let _ = L.iter print_str_nl @@ L.map (show(states)) @@ correct_answers 10;;
  *)

end;;
