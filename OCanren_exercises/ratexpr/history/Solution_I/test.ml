(** Below are some tests *)

open Logic;;
open Core;;
open Ratexpr;;
open LoNat;;
  
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


(* subtraction not efficient *)
let _ =  let open Inj in print_string
    "[eval] basic forward : (Prod (Subt(Num(3,7),Num(1,15)),Sum(Num(2,1),Num(5,2))))\n";
  List.iter (fun p -> print_string @@ GT.show(frat) p; print_newline() )
  @@ RStream.take ~n:1 @@
  run one ( fun p -> ocanren {
      eval (Prod (Subt(Num(3,7),Num(1,15)), Sum(Num(2,1),Num(5,2)))) p})
    (fun p  -> GRat.to_frat @@ project p ) ;;



(*

let _ =  print_string "[radd] basic backward : radd a b 1 5 12 35\n";
  List.iter (fun p -> print_string @@ GT.show(pr) p; print_newline() )
  @@ RStream.take ~n:1 @@
  run two ( fun a b -> ocanren {radd a b 1 5 12 35})
    (fun a b  -> LNat.to_int @@ project a, LNat.to_int @@ project b ) ;;

let _ =  print_string "[radd] basic backward : radd a b 1 7 12 35\n";
  List.iter (fun p -> print_string @@ GT.show(pr) p; print_newline() )
  @@ RStream.take ~n:1 @@
  run two ( fun a b -> ocanren { radd a b 1 7 12 35})
    (fun a b  -> LNat.to_int @@ project a, LNat.to_int @@ project b ) ;;

let _ =  print_string "[radd] basic backward : radd a b 4 15 14 15\n";
  List.iter (fun p -> print_string @@ GT.show(pr) p; print_newline() )
  @@ RStream.take ~n:1 @@
  run two ( fun a b -> ocanren { radd  a b 4 15 14 15})
    (fun a b  -> LNat.to_int @@ project a, LNat.to_int @@ project b ) ;;


let _ =  print_string "[radd] basic backward : radd a b 2 3 14 15\n";
  List.iter (fun p -> print_string @@ GT.show(pr) p; print_newline() )
  @@ RStream.take ~n:1 @@
  run two ( fun a b -> ocanren { radd a b 2 3 14 15})
    (fun a b  -> LNat.to_int @@ project a, LNat.to_int @@ project b ) ;;


let _ =  print_string "[rmul] basic forward : rmul 1 5 3 7 a b\n";
  List.iter (fun p -> print_string @@ GT.show(pr) p; print_newline() )
  @@ RStream.take ~n:1 @@
  run two ( fun a b -> ocanren {rmul 1 5 3 7 a b})
    (fun a b  -> LNat.to_int @@ project a, LNat.to_int @@ project b ) ;;


(* inefficient *)
let _ =  print_string "[rmul] basic backward : rmul 1 5 a b 3 7 \n";
  List.iter (fun p -> print_string @@ GT.show(pr) p; print_newline() )
  @@ RStream.take ~n:1 @@
  run two ( fun a b -> ocanren {rmul 1 5 a b 3 7 })
    (fun a b  -> LNat.to_int @@ project a, LNat.to_int @@ project b ) ;;

(* for small rational the search is much faster *)
let _ =  print_string "[radd_gt_rm] basic backward : radd_gt_rm 2 3 a b 7 6\n";
  List.iter (fun p -> print_string @@ GT.show(pr) p; print_newline() )
  @@ RStream.take ~n:1 @@
  run two ( fun a b -> ocanren { radd_gt_rm 2 3 a b 7 6})
    (fun a b  -> LNat.to_int @@ project a, LNat.to_int @@ project b ) ;;


(* slow search *)
let _ =  print_string "[radd_gt_rm] basic backward : radd_gt_rm 2 13 a b 35 143\n";
  List.iter (fun p -> print_string @@ GT.show(pr) p; print_newline() )
  @@ RStream.take ~n:1 @@
  run two ( fun a b -> ocanren { radd_gt_rm 2 13 a b 9 143})
    (fun a b  -> LNat.to_int @@ project a, LNat.to_int @@ project b ) ;;


(* refutationally incomplete *)
let _ =  print_string "[radd_gt_rm] basic backward : radd_gt_rm 2 13 a b 9 143\n";
  List.iter (fun p -> print_string @@ GT.show(pr) p; print_newline() )
  @@ RStream.take ~n:1 @@
  run two ( fun a b -> ocanren { radd_gt_rm 2 13 a b 9 143})
    (fun a b  -> LNat.to_int @@ project a, LNat.to_int @@ project b ) ;;


let _ =  print_string "[radd_gt_rm] basic backward : radd_gt_rm  36 15 a b 5 2\n";
  List.iter (fun p -> print_string @@ GT.show(pr) p; print_newline() )
  @@ RStream.take ~n:1 @@
  run two ( fun a b -> ocanren { radd_gt_rm 36 15 a b 5 2})
    (fun a b  -> LNat.to_int @@ project a, LNat.to_int @@ project b ) ;;


let _ =  print_string " div a 10 b c \n";
  List.iter (fun p -> print_string @@ GT.show(pr3) p; print_newline() )
  @@ RStream.take ~n:40 @@
  run three ( fun  a b c -> ocanren {div a 10 b c})
    (fun a b c -> LNat.to_int @@ project a, LNat.to_int @@ project b,
    LNat.to_int @@ project c) ;;

let _ =  print_string " div a b 10 c \n";
  List.iter (fun p -> print_string @@ GT.show(pr3) p; print_newline() )
  @@ RStream.take ~n:40 @@
  run three ( fun  a b c -> ocanren { div a b 10 c})
    (fun a b c -> LNat.to_int @@ project a, LNat.to_int @@ project b,
    LNat.to_int @@ project c) ;;

(* inefficient search: when b is guessed right (15), cm' is guessed for 360 times *)
let _ =  print_string "[radd_gt_rm] basic backward : radd_gt_rm a b 1 10 5 2\n";
  List.iter (fun p -> print_string @@ GT.show(pr) p; print_newline() )
  @@ RStream.take ~n:1 @@
  run two ( fun a b -> ocanren { radd_gt_rm a b 1 10 5 2})
    (fun a b  -> LNat.to_int @@ project a, LNat.to_int @@ project b ) ;;


let _ =  print_string "[radd_gt_rm] check : radd_gt_rm 36 15 1 10 5 2\n";
  List.iter (fun p -> print_string @@ GT.show(LNat.logic) p; print_newline() )
  @@ RStream.take ~n:1 @@
  run one ( fun a -> ocanren { radd_gt_rm 36 15 1 10 5 2})
    (fun a  -> a#reify(LNat.reify) ) ;;


let _ =  print_string "[=/=, div] c =/= 0 & div 12 a b c \n";
  List.iter (fun p -> print_string @@ GT.show(lnp3) p; print_newline() )
  @@ RStream.take  @@
  run three ( fun  a b c -> ocanren { c =/= 0 & div 12 a b c})
    (fun a b c ->  a#reify(LNat.reify),  b#reify(LNat.reify),
                    c#reify(LNat.reify)  ) ;;


let _ =  print_string "[=/=] gen: c =/= 0 \n";
  List.iter (fun p -> print_string @@ GT.show(LNat.logic) p; print_newline() )
  @@ RStream.take ~n:10 @@
  run one ( fun  c -> ocanren { c =/= 0 })
    (fun  c -> c#reify(LNat.reify) ) ;;



(* genrated many before long *)
let _ =  print_string "[radd_gt_dv] advanced backward : radd_gt_dv  a b c d 5 2\n";
  List.iter (fun p -> print_string @@ GT.show(pr4) p; print_newline() )
  @@ RStream.take ~n:10 @@
  run four ( fun a b c d -> ocanren { radd_gt_dv a b c d 5 2})
    (fun a b c d -> LNat.to_int @@ project a, LNat.to_int @@ project b,
    LNat.to_int @@ project c, LNat.to_int @@ project d) ;;


(* no problem if you ask for one nswer, if you ask more, it diverges *)
let _ =  print_string "[radd_gt_dv] basic backward : radd_gt_dv a b 1 10 5 2\n";
  List.iter (fun p -> print_string @@ GT.show(pr) p; print_newline() )
  @@ RStream.take ~n:1 @@
  run two ( fun a b -> ocanren { radd_gt_dv a b 1 10 5 2})
    (fun a b  -> LNat.to_int @@ project a, LNat.to_int @@ project b ) ;;

let _ =  print_string "[radd_gt_dv] basic backward : radd_gt_dv 1 10 a b 5 2\n";
  List.iter (fun p -> print_string @@ GT.show(pr) p; print_newline() )
  @@ RStream.take ~n:1 @@
  run two ( fun a b -> ocanren { radd_gt_dv 1 10 a b 5 2})
    (fun a b  -> LNat.to_int @@ project a, LNat.to_int @@ project b ) ;;


let _ =  print_string "[=/=, div] c =/= 0 & div a 10 b c \n";
  List.iter (fun p -> print_string @@ GT.show(pr3) p; print_newline() )
  @@ RStream.take ~n:10 @@
  run three ( fun  a b c -> ocanren { c =/= 0 & div a 10 b c})
    (fun a b c -> LNat.to_int @@ project a, LNat.to_int @@ project b,
    LNat.to_int @@ project c) ;;



(* finds all answers and does not diverge, better than remainder ! *)
let _ =  print_string "[LoNat.div] forward + gen: div 5 a q r \n";
  List.iter (fun p -> print_string @@ GT.show(lnp3) p; print_newline() )
  @@ RStream.take ~n:10 @@
  run three ( fun  a q r -> ocanren {div 5 a q r })
    (fun a q r -> a#reify(LNat.reify) , q#reify(LNat.reify), r#reify(LNat.reify) ) ;;

let _ =  print_string "[LoNat.div] forward + gen: div 10 a q 0 \n";
  List.iter (fun p -> print_string @@ GT.show(pr) p; print_newline() )
  @@ RStream.take ~n:10 @@
  run two ( fun  a q -> ocanren {div 10 a q 0 })
    (fun a q -> LNat.to_int @@ project a, LNat.to_int @@ project q ) ;;


let _ =  print_string "[LNat.( * )] gen: ( * ) 10 a b \n";
  List.iter (fun p -> print_string @@ GT.show(pr) p; print_newline() )
  @@ RStream.take ~n:10 @@
  run two ( fun  a b -> ocanren {LNat.( * ) 10 a b })
    (fun  q r ->  LNat.to_int @@ project q, LNat.to_int @@ project r ) ;;

let _ =  print_string "[radd_gt_dv] basic forward : radd_gt_dv 1 10 5 2 a b\n";
  List.iter (fun p -> print_string @@ GT.show(pr) p; print_newline() )
  @@ RStream.take ~n:3 @@
  run two ( fun a b -> ocanren { radd_gt_dv 1 10 5 2 a b })
    (fun a b  -> LNat.to_int @@ project a, LNat.to_int @@ project b ) ;;

let _ =  print_string "[LNat.( * )] refute: ( * ) 71 c 60 \n";
  List.iter (fun p -> print_string @@ GT.show(GT.int) p; print_newline() )
  @@ RStream.take ~n:10 @@
  run one ( fun  c -> ocanren {LNat.( * ) 71 c 60 })
    (fun  c -> LNat.to_int @@ project c ) ;;


(* when ~n:6 gives all answers but if ~n:7 then diverge *)
let _ =  print_string "[remainder] forward + gen : remainder 5 a b\n";
  List.iter (fun p -> print_string @@ GT.show(lnp) p; print_newline() )
  @@ RStream.take ~n:6 @@
  run two ( fun a b -> ocanren { remainder 5 a b })
    (fun a b  ->  a#reify(LNat.reify) , b#reify(LNat.reify) ) ;;


(* slow *)
let _ =  print_string "[radd_gt] subtruct for arg2 : radd_gt 1 5 a b 1 2\n";
  List.iter (fun p -> print_string @@ GT.show(pr) p; print_newline() )
  @@ RStream.take ~n:3 @@
  run two ( fun a b -> ocanren { radd_gt  1 5 a b 1 2 })
    (fun a b  -> LNat.to_int @@ project a, LNat.to_int @@ project b ) ;;



let _ =  print_string "[LNat.( * )] forward + gen: ( * ) 2 a q \n";
  List.iter (fun p -> print_string @@ GT.show(pr) p; print_newline() )
  @@ RStream.take ~n:10 @@
  run two ( fun  a q -> ocanren {LNat.( * ) 2 a q })
    (fun a q  -> LNat.to_int @@ project a, LNat.to_int @@ project q) ;;


let _ =  print_string "[LoNat.div] backward: div 5 a q 0 \n";
  List.iter (fun p -> print_string @@ GT.show(pr) p; print_newline() )
  @@ RStream.take ~n:10 @@
  run two ( fun  a q -> ocanren {div 5 a q 0 })
    (fun a q  -> LNat.to_int @@ project a, LNat.to_int @@ project q) ;;


(* very good, find why? *)
let _ =  print_string "[radd_gt] backward : radd_gt a b 1 5 1 2\n";
  List.iter (fun p -> print_string @@ GT.show(pr) p; print_newline() )
  @@ RStream.take ~n:3 @@
  run two ( fun a b -> ocanren { radd_gt a b 1 5 1 2 })
    (fun a b  -> LNat.to_int @@ project a, LNat.to_int @@ project b ) ;;


(* more care here ! *)
let _ =  print_string "[radd_gt] backward : radd_gt a b c d 1 2\n";
  List.iter (fun p -> print_string @@ GT.show(pr4) p; print_newline() )
  @@ RStream.take ~n:3 @@
  run four ( fun a b c d-> ocanren {fresh r in radd_gt a b c d 16 19 & remainder b d r & r =/= 0 })
    (fun a b c d -> LNat.to_int @@ project a, LNat.to_int @@ project b,
                    LNat.to_int @@ project c ,LNat.to_int @@ project d ) ;;


let _ =  print_string "[LoNat.div] backward: div a 4 3 0 \n";
  List.iter (fun p -> print_string @@ GT.show(GT.int) p; print_newline() )
  @@ RStream.take ~n:10 @@
  run one ( fun  a -> ocanren {div a 4 3 0 })
    (fun a  -> LNat.to_int @@ project a) ;;


let _ =  print_string "[LoNat.div] backward: div a b 3 0 \n";
  List.iter (fun p -> print_string @@ GT.show(pr) p; print_newline() )
  @@ RStream.take ~n:10 @@
  run two ( fun  a b -> ocanren {div a b 3 0 })
    (fun a b -> LNat.to_int @@ project a, LNat.to_int @@ project b) ;;


let _ =  print_string "[LoNat.div] scale: div a 3 b 0 \n";
  List.iter (fun p -> print_string @@ GT.show(pr) p; print_newline() )
  @@ RStream.take ~n:10 @@
  run two ( fun  a b -> ocanren {div a 3 b 0 })
    (fun a b -> LNat.to_int @@ project a, LNat.to_int @@ project b) ;;

let _ =  print_string "[remainder] backward: remainder a b r [r =/= 0] \n";
  List.iter (fun p -> print_string @@ GT.show(lnp) p; print_newline() )
  @@ RStream.take ~n:10 @@
  run two ( fun  a b -> ocanren {fresh r in remainder a b r & r =/= 0})
    (fun a b -> a#reify(LNat.reify) , b#reify(LNat.reify) ) ;;


let _ =  print_string "[LoNat.div] factor: div 71 a b 0 \n";
  List.iter (fun p -> print_string @@ GT.show(pr) p; print_newline() )
  @@ RStream.take ~n:10 @@
  run two ( fun  a b -> ocanren {div 71 a b 0 })
    (fun a b -> LNat.to_int @@ project a, LNat.to_int @@ project b) ;;


let _ =  print_string "[LNat.( * )] refute: ( * ) 71 c 60 \n";
  List.iter (fun p -> print_string @@ GT.show(GT.int) p; print_newline() )
  @@ RStream.take ~n:10 @@
  run one ( fun  c -> ocanren {LNat.( * ) 71 c 60 })
    (fun  c -> LNat.to_int @@ project c ) ;;


let _ =  print_string "[LoNat.lcm] forward: lcm 12 5 c \n";
  List.iter (fun p -> print_string @@ GT.show(GT.int) p; print_newline() )
  @@ RStream.take ~n:10 @@
  run one ( fun  c -> ocanren {lcm 12 5 c })
    (fun  c -> LNat.to_int @@ project c ) ;;

let _ =  print_string "[LoNat.div] refute: div 12 5 c 0 \n";
  List.iter (fun p -> print_string @@ GT.show(LNat.logic) p; print_newline() )
  @@ RStream.take ~n:10 @@
  run one ( fun  c -> ocanren {div 12 5 c 0 })
    (fun  c ->  c#reify(LNat.reify)  ) ;;

let _ =  print_string "[radd_gt] forward : radd_gt 0 99 0 1 a b\n";
  List.iter (fun p -> print_string @@ GT.show(pr) p; print_newline() )
  @@ RStream.take ~n:3 @@
  run two ( fun a b -> ocanren {radd_gt 0 99 0 1 a b})
    (fun a b -> LNat.to_int @@ project a, LNat.to_int @@ project b ) ;;

let _ =  print_string "[radd_ed] forward : radd_ed 7 20 13 20 a b\n";
  List.iter (fun p -> print_string @@ GT.show(pr) p; print_newline() )
  @@ RStream.take ~n:3 @@
  run two ( fun a b -> ocanren {radd_ed 7 20 13 20 a b})
    (fun a b -> LNat.to_int @@ project a, LNat.to_int @@ project b ) ;;

let _ =  print_string "[radd_gt] forward : radd_gt 7 12 3 5 a b\n";
  List.iter (fun p -> print_string @@ GT.show(pr) p; print_newline() )
  @@ RStream.take ~n:2 @@ (* ~n > 1 is slow *)
  run two ( fun a b -> ocanren {radd_gt 7 12 3 5 a b})
    (fun a b  -> LNat.to_int @@ project a, LNat.to_int @@ project b ) ;;



let _ =  print_string "[LoNat.div] generate: div a b c 0 \n";
  List.iter (fun p -> print_string @@ GT.show(lnp3) p; print_newline() )
  @@ RStream.take ~n:10 @@
  run three ( fun a b c -> ocanren {div a b c 0 })
    (fun a b c ->  a#reify(LNat.reify),  b#reify(LNat.reify),
                    c#reify(LNat.reify)  ) ;;


let _ =  print_string "[LoNat.div] generate: div a b c d \n";
  List.iter (fun p -> print_string @@ GT.show(lnp4) p; print_newline() )
  @@ RStream.take ~n:5 @@
  run four ( fun a b c d-> ocanren {div a b c d })
    (fun a b c d ->  a#reify(LNat.reify),  b#reify(LNat.reify),
                    c#reify(LNat.reify) , d#reify(LNat.reify) ) ;;

let _ =  print_string "[LoNat.div] backward : div x y 5 3\n";
  List.iter (fun p -> print_string @@ GT.show(pr) p; print_newline() )
  @@ RStream.take ~n:10 @@
  run two ( fun x y -> ocanren { div x y 5 3 })
    (fun a b -> LNat.to_int @@ project a, LNat.to_int @@ project b ) ;;

let _ =  print_string "[LoNat.div] forward : div 17 5 q r\n";
  List.iter (fun p -> print_string @@ GT.show(pr) p; print_newline() )
  @@ RStream.take ~n:2 @@
  run two ( fun q r -> ocanren { div 17 5 q r })
    (fun a b -> LNat.to_int @@ project a, LNat.to_int @@ project b ) ;;

let _ =  print_string "[LoNat.div] forward : div 177 59 q r\n";
  List.iter (fun p -> print_string @@ GT.show(pr) p; print_newline() )
  @@ RStream.take ~n:2 @@
  run two ( fun q r -> ocanren { div 177 59 q r })
    (fun a b -> LNat.to_int @@ project a, LNat.to_int @@ project b ) ;;


let _ =  print_string "[Bounded.radd] check : Bounded.radd 3 10 1 5 1 2\n";
  List.iter (fun p -> print_string @@ GT.show(LNat.logic) p; print_newline() )
  @@ RStream.take ~n:1 @@
  run one ( fun a -> ocanren { Bounded.radd 3 10 1 5 1 2})
    (fun a -> a#reify(LNat.reify)) ;;

let _ =  print_string "[Bounded.radd] backward : Bounded.radd a b c d 1 2\n";
  List.iter (fun p -> print_string @@ GT.show(pr4) p; print_newline() )
  @@ RStream.take ~n:60 @@
  run four ( fun a b c d-> ocanren { Bounded.radd a b c d 1 2})
    (fun a b c d -> LNat.to_int @@ project a, LNat.to_int @@ project b,
                    LNat.to_int @@ project c ,LNat.to_int @@ project d ) ;;

let _ =  print_string "[Bounded.radd] forward : Bounded.radd 2 5 1 4 a b\n";
  List.iter (fun p -> print_string @@ GT.show(pr) p; print_newline() )
  @@ RStream.take ~n:1 @@
  run two ( fun a b -> ocanren { Bounded.radd 2 5 1 4 a b})
    (fun a b -> LNat.to_int @@ project a, LNat.to_int @@ project b ) ;;


let _ =  print_string "[radd_core] backward : radd_core a b c d 1 3\n";
  List.iter (fun p -> print_string @@ GT.show(pr4) p; print_newline() )
  @@ RStream.take ~n:10 @@
  run four ( fun a b c d-> ocanren { radd_core a b c d 1 3})
    (fun a b c d -> LNat.to_int @@ project a, LNat.to_int @@ project b,
    LNat.to_int @@ project c ,LNat.to_int @@ project d ) ;;

let _ =  print_string "[radd_core] backward : radd_core a b c d 3 2\n";
  List.iter (fun p -> print_string @@ GT.show(pr4) p; print_newline() )
  @@ RStream.take ~n:10 @@
  run four ( fun a b c d-> ocanren {radd_core a b c d 3 2})
    (fun a b c d -> LNat.to_int @@ project a, LNat.to_int @@ project b,
    LNat.to_int @@ project c ,LNat.to_int @@ project d ) ;;


let _ =  print_string "[Bounded.radd_core] backward : Bounded.radd_core a b c d 1 3\n";
  List.iter (fun p -> print_string @@ GT.show(pr4) p; print_newline() )
  @@ RStream.take  @@
  run four ( fun a b c d-> ocanren { Bounded.radd_core a b c d 1 3})
    (fun a b c d -> LNat.to_int @@ project a, LNat.to_int @@ project b,
    LNat.to_int @@ project c ,LNat.to_int @@ project d ) ;;

let _ =  print_string "[Bounded.radd_core_bd] backward : Bounded.radd_core_bd a b c d 3 2\n";
  List.iter (fun p -> print_string @@ GT.show(pr4) p; print_newline() )
  @@ RStream.take  @@
  run four ( fun a b c d-> ocanren {Bounded.radd_core a b c d 3 2})
    (fun a b c d -> LNat.to_int @@ project a, LNat.to_int @@ project b,
    LNat.to_int @@ project c ,LNat.to_int @@ project d ) ;;


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

*)

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



