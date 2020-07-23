open GT;;
open OCanren;;
module L = List;;
open OCanren.Std;;

module Rat = LRational;;

@type mine = A | B with show;;

module Machine = struct
  open Rat;;
  
  (* machine performance on A *)
  let p = inj_int_ratio (1,5)
  and r = inj_int_ratio (1,2);;

  (* machine performance on B *)
  let q = inj_int_ratio (3,10)
  and s = inj_int_ratio (33,100);;
end;;

module Mine = struct
  let a = !!A         (* injected *)
  and b = !!B;;
  
  open Rat;;
  
  let x = inj_int_ratio (100,1)     (* init amount in A *)
  and y = inj_int_ratio (120,1);;   (* init amount in B *)
end;;

let select m a b sel =
  ocanren {
    m == Mine.a & a == sel |
    m == Mine.b & b == sel
  };;


let rec expectation amt_A amt_B plan (expc : Rat.groundi)=
  ocanren {
    plan == [] & expc == (0,1) |
    fresh m, ms,pr, fr,
          amt_now, amt_mined, amt_left in
      plan == m :: ms                  &
      select m Machine.p Machine.q pr  &
      select m Machine.r Machine.s fr  &
      select m amt_A amt_B amt_now     &
      Rat.( * ) fr amt_now amt_mined   &
      Rat.( - ) amt_now amt_mined amt_left & 
    fresh summ, expc_ms, expc_ms', expc_ms'' in
     Rat.( + ) amt_mined expc_ms summ &
     Rat.( * ) pr summ expc           &
     select m expc_ms' expc_ms'' expc &
     expectation amt_left amt_B ms expc_ms' 
     expectation amt_A amt_left ms expc_ms'' (* this double recursion is a source 
                                                of inefficiency *)
};;

let rec expectation' hd_plan amt_A amt_B tl_plan (expc : Rat.groundi)=
  ocanren {
    fresh amt_mined in
     {hd_plan == Mine.a & Rat.( * ) Machine.r amt_A amt_mined |
      hd_plan == Mine.b & Rat.( * ) Machine.s amt_B amt_mined }
     &
     { tl_plan == [] & 
         { hd_plan == Mine.a & Rat.( * ) Machine.p amt_mined expc
           hd_plan == Mine.b & Rat.( * ) Machine.q amt_mined expc }
     | fresh m, ms in tl_plan == m :: ms &
         fresh amt_left, expc_tl, summ in
         { hd_plan == Mine.a & Rat.( - ) amt_A amt_mined amt_left
           & expectation' m amt_left amt_B ms expc_tl
           & Rat.( + ) amt_mined expc_tl summ
           & Rat.( * ) Machine.p summ expc |
           hd_plan == Mine.b & Rat.( - ) amt_B amt_mined amt_left
           & expectation' m  amt_A amt_left ms expc_tl
           & Rat.( + ) amt_mined expc_tl summ
           & Rat.( * ) Machine.q summ expc } } };;


(* some tests *)

@type ipr = int * int with show;;

let _ =
  print_string @@ (show(ipr)) @@ L.hd @@ Stream.take ~n:1 @@
  run q (fun q -> ocanren { Rat.mulo (2,3) (5,7) q }) (Rat.prj_rat);
  print_newline () ;;

let _ =
  print_string @@ (show(ipr)) @@ L.hd @@ Stream.take ~n:1 @@
  run q (fun q -> ocanren { Rat.mulo (2,3) q (10,21)}) (Rat.prj_rat);
   print_newline () ;;

let _ =
  print_string @@ show(ipr) @@ L.hd @@ Stream.take ~n:1 @@
  run q (fun q-> ocanren {Rat.( + ) (1,3) (1,1) q} )  Rat.prj_rat;
  print_newline () ;;

let _ =
  print_string @@ show(ipr) @@ L.hd @@ Stream.take ~n:1 @@
  run q (fun q-> ocanren {Rat.( - ) (3,3) (1,3) q} )  Rat.prj_rat;
  print_newline () ;; 

let _ = let open Mine in
  print_string @@ show(ipr) @@ L.hd @@ Stream.take ~n:1 @@
  run q (fun q-> ocanren {expectation x y [b] q} )  Rat.prj_rat;
  print_newline () ;; 


(*
(* problematic cases *)

@type iprprl = ((int * int) * (int * int)) GT.list with show;;

(* this divergea *)

let _ =
  print_string @@ (show(iprprl)) @@ Stream.take ~n:5 @@
  run qr (fun q r-> ocanren { Rat.mulo q r (10,21)}) (fun q r -> Rat.(prj_rat q, prj_rat r));
  print_newline () ;;

(* This diverges as well. Furthermore, 
   if (3,3) is changed to (1,1) then no answer *)
*)


(* Unfinished *)
