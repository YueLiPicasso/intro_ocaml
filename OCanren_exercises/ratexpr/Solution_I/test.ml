(** Below are some tests *)

open Logic;;
open Core;;
open Ratexpr;;
open LoNat;;
open LoRat;;
  
@type pr = GT.int * GT.int with show;;
@type pr3 =  GT.int * GT.int * GT.int with show;; 
@type intl = GT.int GT.list with show;;
@type ipl = (GT.int * GT.int) GT.list with show;;
@type ipl4 = (GT.int * GT.int * GT.int * GT.int) GT.list with show;;

(** Mixed free variables and ground values are captured by type [logic] *)
@type lnp = LNat.logic * LNat.logic with show;;
@type lnp3 = LNat.logic * LNat.logic * LNat.logic with show;;
@type lnp4 = LNat.logic * LNat.logic * LNat.logic * LNat.logic with show;;
@type lnp4' = LNat.logic * LNat.logic * LNat.logic * GT.int with show;;






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



