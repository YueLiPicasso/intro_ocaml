open GT;;
open OCanren;;
module L = List;;
open OCanren.Std;;

module Rat = LRational;;

@type mine = A | B with show;;
@type plan = mine GT.list with show;;

module Machine = struct
  open Rat;;
  (* machine performance on A *)
  let p = inj_int_ratio (1,3)
  and r = inj_int_ratio (1,2);;
  (* machine performance on B *)
  let q = inj_int_ratio (1,2)
  and s = inj_int_ratio (1,3);;
end;;

module Machine' = struct
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
  let x = inj_int_ratio (1,1)     (* init amount in A *)
  and y = inj_int_ratio (2,1);;   (* init amount in B *)

  let prj_plan x = List.to_list id @@ project x
end;;

module Mine' = struct
  let a = !!A         (* injected *)
  and b = !!B;;
  open Rat;;
  let x = inj_int_ratio (100,1)     (* init amount in A *)
  and y = inj_int_ratio (120,1);;   (* init amount in B *)

  let prj_plan x = List.to_list id @@ project x
end;;


module Compute = struct
  
  (* Expectation for (hd_plan :: tl_plan) *)
  let rec expectation' hd_plan amt_A amt_B tl_plan (expc : Rat.groundi) =
    ocanren {
      fresh amt_mined in
       {hd_plan == Mine.a & Rat.( * ) Machine.r amt_A amt_mined |
        hd_plan == Mine.b & Rat.( * ) Machine.s amt_B amt_mined }
       &
     { tl_plan == [] & 
     { hd_plan == Mine.a & Rat.( * ) Machine.p amt_mined expc |
       hd_plan == Mine.b & Rat.( * ) Machine.q amt_mined expc }
     | fresh m, ms in tl_plan == m :: ms &
         fresh amt_left, expc_tl, summ in
         { hd_plan == Mine.a & Rat.( - ) amt_A amt_mined amt_left
           & Rat.( + ) amt_mined expc_tl summ
           & Rat.( * ) Machine.p summ expc
           & expectation' m amt_left amt_B ms expc_tl |
           hd_plan == Mine.b & Rat.( - ) amt_B amt_mined amt_left
           & Rat.( + ) amt_mined expc_tl summ
           & Rat.( * ) Machine.q summ expc
           & expectation' m  amt_A amt_left ms expc_tl } } };;

  (* Expectation for (hd_plan :: tl_plan), clauses reordered *)
  let rec expectation'' hd_plan amt_A amt_B tl_plan (expc : Rat.groundi) =
  ocanren {
    fresh amt_mined in
      hd_plan == Mine.a &
      Rat.( * ) Machine.r amt_A amt_mined &
      { tl_plan == [] & Rat.( * ) Machine.p amt_mined expc |
        fresh m, ms in tl_plan == m :: ms &
          fresh amt_left, expc_tl, summ in
            Rat.( - ) amt_A amt_mined amt_left
          & Rat.( + ) amt_mined expc_tl summ
          & Rat.( * ) Machine.p summ expc
          & expectation'' m amt_left amt_B ms expc_tl }
    | hd_plan == Mine.b &
      Rat.( * ) Machine.s amt_B amt_mined &
      { tl_plan == [] & Rat.( * ) Machine.q amt_mined expc |
        fresh m, ms in tl_plan == m :: ms &
          fresh amt_left, expc_tl, summ in
            Rat.( - ) amt_B amt_mined amt_left
          & Rat.( + ) amt_mined expc_tl summ
          & Rat.( * ) Machine.q summ expc
          & expectation'' m  amt_A amt_left ms expc_tl } };;
end;;


let expectation amt_A amt_B plan (expc : Rat.groundi) =
  let open Compute in
  ocanren {
    plan == [] & expc == (0,1) |
    fresh m, ms in plan == m :: ms &
      expectation'' m amt_A amt_B ms expc };;

let expectationn amt_A amt_B plan (expc : Rat.groundi) =
  let open Compute in
  ocanren {
    plan == [] & expc == (0,1) |
    fresh m, ms in plan == m :: ms &
      expectation' m amt_A amt_B ms expc };;


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

@type iprprl = ((int * int) * (int * int)) GT.list with show;;

let _ =                     (* this diverges if ~n>16 *)
  print_string @@ (show(iprprl)) @@ Stream.take ~n:16 @@
  run qr (fun q r-> ocanren { Rat.mulo q r (10,21)}) (fun q r -> Rat.(prj_rat q, prj_rat r));
  print_newline () ;;

let _ = let open Mine in
  print_string @@ show(ipr) @@ L.hd @@ Stream.take ~n:1 @@
  run q (fun q-> ocanren {expectation x y [a] q} )  Rat.prj_rat;
  print_newline () ;; 

let _ = let open Mine in
  print_string @@ show(ipr) @@ L.hd @@ Stream.take ~n:1 @@
  run q (fun q-> ocanren {expectation x y [b] q} )  Rat.prj_rat;
  print_newline () ;; 

(*
let _ = let open Mine in
  print_string @@ show(plan) @@ L.hd @@ Stream.take ~n:1 @@
  run q (fun q-> ocanren {expectation x y q (11880, 1000)} )  prj_plan;
  print_newline () ;; *)

(* Not sure if this diverges or is just slow: it should not diverge though *)
let _ = let open Mine in
  print_string @@ show(ipr) @@ L.hd @@ Stream.take ~n:1 @@
  run q (fun q-> ocanren {expectationn x y [b;a;b;a;b;b] q} )  Rat.prj_rat;
  print_newline () ;; 

(* only gives [] *)
let _ = let open Mine in
  print_string @@ show(plan) @@ L.hd @@ Stream.take ~n:3 @@
  run q (fun q-> ocanren {fresh ex in expectation x y q ex} )  prj_plan;
  print_newline () ;; 

(* only gives [] *)
let _ = let open Mine in
  print_string @@ show(plan) @@ L.hd @@ Stream.take ~n:3 @@
  run q (fun q-> ocanren {fresh ex in expectationn x y q ex} )  prj_plan;
  print_newline () ;; 



