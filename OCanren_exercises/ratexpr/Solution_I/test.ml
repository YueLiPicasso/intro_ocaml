(** Below are some tests *)

open Logic;;
open Core;;
open Ratexpr;;
open LoNat;;
open LoRat;;
  
@type pr  = GT.int * GT.int with show;;
@type pr3 =  GT.int * GT.int * GT.int with show;;
@type pr4 =  GT.int * GT.int * GT.int * GT.int with show;;

@type ipl  = GT.int GT.list with show;;
@type ipl2 = (GT.int * GT.int) GT.list with show;;
@type ipl3 = (GT.int * GT.int * GT.int) GT.list with show;;
@type ipl4 = (GT.int * GT.int * GT.int * GT.int) GT.list with show;;

@type lnp   = LNat.logic * LNat.logic with show;;
@type lnp3  = LNat.logic * LNat.logic * LNat.logic with show;;
@type lnp4  = LNat.logic * LNat.logic * LNat.logic * LNat.logic with show;;



let _ =  print_string "[simplify_bd] generate : simplify_bd a b c\n";
  List.iter (fun p -> print_string @@ GT.show(pr4) p; print_newline() )
  @@ RStream.take ~n:2500 @@
  run four ( fun a b c d-> ocanren { simplify_bd a b c d})
    (fun a b c d -> LNat.to_int @@ project a, LNat.to_int @@ project b,
    LNat.to_int @@ project c ,LNat.to_int @@ project d ) ;;



(* 1000 answers ok *)
let _ =  print_string "[gcd_bd] generate : gcd_bd a b c\n";
  List.iter (fun p -> print_string @@ GT.show(pr3) p; print_newline() )
  @@ RStream.take ~n:5 @@
  run three ( fun a b c -> ocanren {  gcd_bd a b c})
    (fun a b c -> LNat.to_int @@ project a, LNat.to_int @@ project b,
    LNat.to_int @@ project c ) ;;

(* 1000 answers ok *)
let _ =  print_string "[lcm_bd] generate : lcm_bd a b c\n";
  List.iter (fun p -> print_string @@ GT.show(pr3) p; print_newline() )
  @@ RStream.take ~n:5 @@
  run three ( fun a b c -> ocanren {lcm_bd a b c})
    (fun a b c -> LNat.to_int @@ project a, LNat.to_int @@ project b,
    LNat.to_int @@ project c ) ;;

let _ =  print_string "[lcm_core] generate : lcm_core a b c\n";
  List.iter (fun p -> print_string @@ GT.show(pr3) p; print_newline() )
  @@ RStream.take ~n:5 @@
  run three ( fun a b c -> ocanren {lcm_core a b c})
    (fun a b c -> LNat.to_int @@ project a, LNat.to_int @@ project b,
    LNat.to_int @@ project c ) ;;


let _ = print_string "[lcm] forward : lcm 18 27 c & lcm 27 18 c\n";
  List.iter (fun p -> print_string @@ GT.show(GT.int) p; print_newline() )
  @@ RStream.take ~n:2 @@
  run q ( fun c -> ocanren {lcm 18 27 c & lcm 27 18 c})
    (fun c -> LNat.to_int @@ project c ) ;;

(* ok *)
let _ = print_string "[radd_core_1] forward [b == b'] : radd_core_1 1 3 4 3 c d\n";
  List.iter (fun p -> print_string @@ GT.show(pr) p; print_newline() )
  @@ RStream.take ~n:1 @@
  run two ( fun c d -> ocanren {radd_core_1 1 3 4 3 c d})
    (fun c d -> LNat.to_int @@ project c, LNat.to_int @@ project d ) ;;

(* quick for 100 answers *)
let _ =  print_string "[radd_core_1] backward r1 r2 missing : radd_core_1 a b c d 2 1\n";
  List.iter (fun p -> print_string @@ GT.show(pr4) p; print_newline() )
  @@ RStream.take ~n:5 @@
  run four ( fun a b c d -> ocanren {radd_core_1 a b c d 2 1})
    (fun a b c d -> LNat.to_int @@ project a, LNat.to_int @@ project b,
    LNat.to_int @@ project c, LNat.to_int @@ project d) ;;



(*

(* ok *)
let _ = print_string "[radd] forward [b =/= b'] : radd 1 2 4 6 c d\n";
  List.iter (fun p -> print_string @@ GT.show(pr) p; print_newline() )
  @@ RStream.take ~n:1 @@
  run two ( fun c d -> ocanren {radd 1 2 4 6 c d})
    (fun c d -> LNat.to_int @@ project c, LNat.to_int @@ project d ) ;;

(* diverge  *)
let _ =  print_string "[radd] backward r2 missing : radd 1 3 c d 2 1\n";
  List.iter (fun p -> print_string @@ GT.show(pr) p; print_newline() )
  @@ RStream.take ~n:10 @@
  run two ( fun c d -> ocanren {radd 1 3 c d 2 1})
    (fun c d -> LNat.to_int @@ project c, LNat.to_int @@ project d ) ;;


(* diverge  *)
let _ =  print_string "[radd] backward r1 missing : radd c d 1 3 2 1\n";
  List.iter (fun p -> print_string @@ GT.show(pr) p; print_newline() )
  @@ RStream.take ~n:10 @@
  run two ( fun c d -> ocanren {radd c d 1 3 2 1})
    (fun c d -> LNat.to_int @@ project c, LNat.to_int @@ project d ) ;;

*)



(*
(** test [eval''_b]: check  *)
let _ =
  let open Inj in 
  List.iter (fun fr -> print_string @@ (GT.show(logic)) fr; print_newline())
  @@ RStream.take ~n:1 @@ 
  run q (fun q ->  ocanren { eval''_b
                               (Sum (Num (3,1), Num (1,1)))
                               (Num (4,1)) })
    (fun q -> q#reify(reify))
;;


(** test [eval''_b]: check  *)
let _ =
  let open Inj in 
  List.iter (fun fr -> print_string @@ (GT.show(logic)) fr; print_newline())
  @@ RStream.take ~n:1 @@ 
  run q (fun q ->  ocanren { eval''_b
                               (Sum (Prod (Num (1,2), Num (2, 1)), Num (3, 1)))
                               (Num (4,1)) })
    (fun q -> q#reify(reify))
;;


(** test [eval''_b]: check  *)
let _ =
  let open Inj in 
  List.iter (fun fr -> print_string @@ (GT.show(logic)) fr; print_newline())
  @@ RStream.take ~n:1 @@ 
  run q (fun q ->  ocanren { eval''_b
                               (Sum (Prod (Num (2, 1), Num (1, 3)),
                                     Sum (Num (1, 3), Num (3, 1))))
                               (Num (4,1)) })
    (fun q -> q#reify(reify))
;;
*)



