open GT;;
open OCanren;;
module L = List;;
open OCanren.Std;;

module Rat = LRational;;

@type mine = A | B with show;;
@type plan = mine GT.list with show;;

(* injection/projection primitives *)
let a = !!A and b = !!B;;
let prj_plan x = List.to_list id @@ project x
    
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
  open Rat;;
  let x = inj_int_ratio (1,1)     (* init amount in A *)
  and y = inj_int_ratio (2,1);;   (* init amount in B *)
end;;

module Mine' = struct
  open Rat;;
  let x = inj_int_ratio (100,1)     (* init amount in A *)
  and y = inj_int_ratio (120,1);;   (* init amount in B *)
end;;


module Compute = struct
  (* Choose which namespaces to open here *)
  open Mine;; open Machine;; 
  (* open Mine';; open Machine';; *)
  
  (* Expectation for (hd_plan :: tl_plan) *)
  let rec expectation' hd_plan amt_A amt_B tl_plan (expc : Rat.groundi) =
    ocanren {
      fresh amt_mined in
       {hd_plan == a & Rat.( * ) r amt_A amt_mined |
        hd_plan == b & Rat.( * ) s amt_B amt_mined }
       &
     { tl_plan == [] & 
     { hd_plan == a & Rat.( * ) p amt_mined expc |
       hd_plan == b & Rat.( * ) q amt_mined expc }
     | fresh m, ms in tl_plan == m :: ms &
         fresh amt_left, expc_tl, summ in
         { hd_plan == a & Rat.( - ) amt_A amt_mined amt_left
           & Rat.( + ) amt_mined expc_tl summ
           & Rat.( * ) p summ expc
           & expectation' m amt_left amt_B ms expc_tl |
           hd_plan == b & Rat.( - ) amt_B amt_mined amt_left
           & Rat.( + ) amt_mined expc_tl summ
           & Rat.( * ) q summ expc
           & expectation' m  amt_A amt_left ms expc_tl } } };;

  (* Expectation for (hd_plan :: tl_plan), factored out hd_plan check *)
  let rec expectation'' hd_plan amt_A amt_B tl_plan (expc : Rat.groundi) =
  ocanren {
    fresh amt_mined in
      hd_plan == a &
      Rat.( * ) r amt_A amt_mined &
      { tl_plan == [] & Rat.( * ) p amt_mined expc |
        fresh m, ms in tl_plan == m :: ms &
          fresh amt_left, expc_tl, summ in
            Rat.( - ) amt_A amt_mined amt_left
          & Rat.( + ) amt_mined expc_tl summ
          & Rat.( * ) p summ expc
          & expectation'' m amt_left amt_B ms expc_tl }
    | hd_plan == b &
      Rat.( * ) s amt_B amt_mined &
      { tl_plan == [] & Rat.( * ) q amt_mined expc |
        fresh m, ms in tl_plan == m :: ms &
          fresh amt_left, expc_tl, summ in
            Rat.( - ) amt_B amt_mined amt_left
          & Rat.( + ) amt_mined expc_tl summ
          & Rat.( * ) q summ expc
          & expectation'' m  amt_A amt_left ms expc_tl } };;

  (* Expectation for (hd_plan :: tl_plan), made recursive calls earlier, tucked in "fresh" *)
  let rec expectation''' hd_plan amt_A amt_B tl_plan (expc : Rat.groundi) =
  ocanren {
   {  hd_plan == a &  fresh amt_mined in
      Rat.( * ) r amt_A amt_mined &
      { tl_plan == [] & Rat.( * ) p amt_mined expc |
        fresh m, ms in tl_plan == m :: ms &
          fresh amt_left, expc_tl, summ in
            Rat.( - ) amt_A amt_mined amt_left
          & expectation''' m amt_left amt_B ms expc_tl
          & Rat.( + ) amt_mined expc_tl summ
          & Rat.( * ) p summ expc } }
  | { hd_plan == b & fresh amt_mined in
      Rat.( * ) s amt_B amt_mined &
      { tl_plan == [] & Rat.( * ) q amt_mined expc |
        fresh m, ms in tl_plan == m :: ms &
          fresh amt_left, expc_tl, summ in
            Rat.( - ) amt_B amt_mined amt_left
          &  expectation''' m  amt_A amt_left ms expc_tl
          & Rat.( + ) amt_mined expc_tl summ
          &  Rat.( * ) q summ expc} } };;

end;;

let expectation1 amt_A amt_B plan (expc : Rat.groundi) =
  let open Compute in
  ocanren {
    plan == [] & expc == (0,1) |
    fresh m, ms in plan == m :: ms &
      expectation' m amt_A amt_B ms expc };;

let expectation2 amt_A amt_B plan (expc : Rat.groundi) =
  let open Compute in
  ocanren {
    plan == [] & expc == (0,1) |
    fresh m, ms in plan == m :: ms &
      expectation'' m amt_A amt_B ms expc };;

let expectation3 amt_A amt_B plan (expc : Rat.groundi) =
  let open Compute in
  ocanren {
    plan == [] & expc == (0,1) |
    fresh m, ms in plan == m :: ms &
      expectation''' m amt_A amt_B ms expc };;


(* some tests *)

@type ipr = int * int with show;;


(* Hopeless *)
let _ = let open Mine in
  print_string @@ show(ipr) @@ L.hd @@ Stream.take ~n:1 @@
  run q (fun q-> ocanren {expectation3 x y [a;b;b;a] q} )  Rat.prj_rat;
  print_newline () ;; 


(* Hopeless *)
let _ = let open Mine in
  print_string @@ show(ipr) @@ L.hd @@ Stream.take ~n:1 @@
  run q (fun q-> ocanren {expectation2 x y [a;b;b;a] q} )  Rat.prj_rat;
  print_newline () ;; 


(* USe (9,9) its OK. *)
let _ =
print_string @@ show(ipr) @@ L.hd @@ Stream.take ~n:1 @@
run q (fun q-> ocanren {Rat.( + ) (1,3) q (3,3)} ) Rat.prj_rat;
print_newline () ;; 

(* 

(* These are OK *)

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

let _ = let open Mine' in (* Using Mine' and Machine' for expectation1 *)
  print_string @@ show(plan) @@ L.hd @@ Stream.take ~n:1 @@
  run q (fun q-> ocanren {expectation1 x y q (11880, 1000)} )  prj_plan;
  print_newline () ;; 


*)

(*
 
 (* These are OK *)

 (* Using Mine and Machine for expectation1/2/3 *)

let _ = let open Mine in
  print_string @@ show(ipr) @@ L.hd @@ Stream.take ~n:1 @@
  run q (fun q-> ocanren {expectation1 x y [a] q} )  Rat.prj_rat;
  print_newline () ;; 

let _ = let open Mine in
  print_string @@ show(ipr) @@ L.hd @@ Stream.take ~n:1 @@
  run q (fun q-> ocanren {expectation1 x y [b] q} )  Rat.prj_rat;
  print_newline () ;; 

let _ = let open Mine in
  print_string @@ show(ipr) @@ L.hd @@ Stream.take ~n:1 @@
  run q (fun q-> ocanren {expectation2 x y [b;a] q} )  Rat.prj_rat;
  print_newline () ;; 

let _ = let open Mine in
  print_string @@ show(ipr) @@ L.hd @@ Stream.take ~n:1 @@
  run q (fun q-> ocanren {expectation3 x y [b;a] q} )  Rat.prj_rat;
  print_newline () ;; 

let _ = let open Mine in
  print_string @@ show(ipr) @@ L.hd @@ Stream.take ~n:1 @@
  run q (fun q-> ocanren {expectation3 x y [a;b] q} )  Rat.prj_rat;
  print_newline () ;; 


*)

let _ = let open Mine in                   (* Hopeless for ~n>3 *)
  L.iter print_string @@ L.map (show(plan)) @@ Stream.take ~n:3 @@
  run q (fun q-> ocanren {fresh ex in expectation1 x y q ex} )  prj_plan;
  print_newline () ;; 

let _ = let open Mine in                   (* Hopeless for ~n>3 *)
  L.iter print_string @@ L.map (show(plan)) @@ Stream.take ~n:3 @@
  run q (fun q-> ocanren {fresh ex in expectation2 x y q ex} )  prj_plan;
  print_newline () ;; 

let _ = let open Mine in                   (* Hopeless for ~n>3 *)
  L.iter print_string @@ L.map (show(plan)) @@ Stream.take ~n:3 @@
  run q (fun q-> ocanren {fresh ex in expectation3 x y q ex} )  prj_plan;
  print_newline () ;; 

(* Hopeless *)
let _ = let open Mine in
  print_string @@ show(ipr) @@ L.hd @@ Stream.take ~n:1 @@
  run q (fun q-> ocanren {expectation2 x y [b;b] q} )  Rat.prj_rat;
  print_newline () ;; 

(* Hopeless *)
let _ = let open Mine in
  print_string @@ show(ipr) @@ L.hd @@ Stream.take ~n:1 @@
  run q (fun q-> ocanren {expectation3 x y [b;b] q} )  Rat.prj_rat;
  print_newline () ;; 





