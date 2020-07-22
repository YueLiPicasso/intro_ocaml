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

(* probability of success *)
let success m pr =
  ocanren {
    m == Mine.a & pr == Machine.p |
    m == Mine.b & pr == Machine.q
  };;

let fraction m fr =
  ocanren {
    m == Mine.a & fr == Machine.r |
    m == Mine.b & fr == Machine.s
  };;

let rec expectation amt_A amt_B plan (expc : Rat.groundi) =
  ocanren {
    plan == [] & expc == (0,1) |
    fresh m, ms,pr, fr,
          amt_now, amt_mined, amt_left in
      plan == m :: ms                  &
      { m == Mine.a & amt_A == amt_now |
        m == Mine.b & amt_B == amt_now }
      & success m pr  & fraction m fr  &
      Rat.( * ) fr amt_now amt_mined   &
      Rat.( + ) amt_mined amt_left amt_now &
    fresh summ, expc_ms in
     { m == Mine.a & expectation amt_left amt_B ms expc_ms |
       m == Mine.b & expectation amt_A amt_left ms expc_ms} &
       Rat.( + ) amt_mined expc_ms summ &
       Rat.( * ) pr summ expc          
};;

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
  run q (fun q-> ocanren {Rat.( + ) (1,3) q (3,3)} )  Rat.prj_rat;
  print_newline () ;; (* if (3,3) -> (1,1) then no answer, else divergence *)

(* Unfinished *)
