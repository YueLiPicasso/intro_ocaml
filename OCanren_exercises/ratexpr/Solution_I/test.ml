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
@type lnp = (LNat.logic * LNat.logic) with show;;
@type lnp3 = (LNat.logic * LNat.logic * LNat.logic) with show;;
@type tmp = ground GT.list with show;;


(** evaluate expr *)
  let open Inj in
  List.iter (fun fr -> print_string @@ (GT.show(frat)) fr; print_newline())
  @@ RStream.take ~n:500 @@ 
  run q (fun q ->
      ocanren { eval
                  (Sum (Num (1, 1), Sum (Num (1, 4), Prod (Num (1, 4), Num (1, 1)))))
                  q }) (fun q -> GRat.to_frat @@ project q)


(** Find expr  that normalizes to 3/2 *)
  let open Inj in
  List.iter (fun fr -> print_string @@ (GT.show(frat)) fr; print_newline())
  @@ RStream.take ~n:50 @@ 
  run q (fun q -> ocanren { eval' q (Num (3,2))}) (fun q -> GRat.to_frat @@ project q)




(*
(** Test [simplify_4]: extremely inefficient *)

(** find numbers that simplify to 3 / 2 *)
let _ = 
  print_string @@ GT.show(ipl) @@ RStream.take ~n:5 @@
  run qr (fun q r -> ocanren { simplify_4 q r 3 2 })
    (fun q r -> LNat.to_int @@ project q, LNat.to_int @@ project r);
  print_newline ();;

(** find the number to which 364 / 420  simplifies *)
let _ = 
  print_string @@ GT.show(ipl) @@ RStream.take ~n:1 @@
  run qr (fun q r -> ocanren { simplify_4 364 420  q r })
    (fun q r -> LNat.to_int @@ project q, LNat.to_int @@ project r);
  print_newline ();;
*)

(*
(** test [comdi]. The answer distribution does not look good: too many zeros *)
let _ =
  let rec conv_to_g = function [] -> [] | (a,b,c) :: t ->
    begin
      try (LoNat.Prj.logic_to_ground a,
                       LoNat.Prj.logic_to_ground b,
                                   LoNat.Prj.logic_to_ground c) ::  conv_to_g t
      with Not_a_value -> conv_to_g t
    end
  in
  List.iter (fun x -> print_string @@ GT.show(pr3) x ; print_newline())
  @@ List.map (fun a,b,c -> LNat.to_int a, LNat.to_int b,LNat.to_int c) @@
  conv_to_g @@ RStream.take ~n:500 @@ let open LNat in
  run qrs (fun q r s -> comdi q r s)
    (fun q r s-> q#reify(LNat.reify),
                 r#reify(LNat.reify),
                 s#reify(LNat.reify));;



(** test [<] . THis generates constrained pairs S...S(O) and S...S(_.1) [_.1 =/= O] *)
let _ = 
  List.iter (fun x -> print_string @@ GT.show(lnp) x ; print_newline())
  @@ RStream.take ~n:50 @@
  run qr (fun q r -> LNat.( < ) q r )
    (fun q r -> q#reify(LNat.reify),
                r#reify(LNat.reify));;

(** Test [simplify_3] *)
(** find what  simplifies  to what. Most of the gernated tuples have 
the forms (0, x, 0, x) or (x, 0, x, 0). This is stil cause by backtracking on 
the last goal, but is better that [simplify']. *)
let _ = 
  print_string @@ GT.show(ipl4) @@ RStream.take ~n:400 @@
  run qrst (fun q r s t-> ocanren { simplify_3 q r s t})
    (fun q r s t -> LNat.to_int @@ project q,
                    LNat.to_int @@ project r,
                    LNat.to_int @@ project s,
                    LNat.to_int @@ project t);
  print_newline ();;


(** Test [simplify'] *)

(** find what  simplifies  to what. Most of the gernated tuples have form (0, x, 0, x).
This is cause by backtracking on the last goal *)
let _ = 
  print_string @@ GT.show(ipl4) @@ RStream.take ~n:400 @@
  run qrst (fun q r s t-> ocanren { simplify' q r s t})
    (fun q r s t -> LNat.to_int @@ project q,
                    LNat.to_int @@ project r,
                    LNat.to_int @@ project s,
                    LNat.to_int @@ project t);
  print_newline ();;


(** find numbers to which 300 / 100  simplifies *)
let _ = 
  print_string @@ GT.show(ipl) @@ RStream.take ~n:11 @@
  run qr (fun q r -> ocanren { simplify' 300 200  q r })
    (fun q r -> LNat.to_int @@ project q, LNat.to_int @@ project r);
  print_newline ();;


(** find numbers that simplify to 3 / 2 *)
let _ = 
  print_string @@ GT.show(ipl) @@ RStream.take ~n:11 @@
  run qr (fun q r -> ocanren { simplify' q r 3 2 })
    (fun q r -> LNat.to_int @@ project q, LNat.to_int @@ project r);
  print_newline ();;


(** forward run *)
let _ = let open Inj in
  print_string @@ GT.show(frat) @@ GRat.to_frat @@ List.hd @@ RStream.take ~n:1 @@
  run q (fun q -> ocanren {eval (Sum (Num (1, 3), Num (4, 5))) q})
    project;
  print_newline();;

(** find bounded numbers that simplify to 3 / 2: 
    stupid generate and test *)
let _ =
  let max_int' = OCanren.Std.nat 10 in
  print_string @@ GT.show(ipl) @@ RStream.take ~n:11 @@
  run qr (fun q r -> ocanren { LNat.( < ) q max_int' & LNat.( < ) r max_int' & simplify q r 3 2 })
    (fun q r -> LNat.to_int @@ project q, LNat.to_int @@ project r);
  print_newline ();;


let rec not_mem e l =
  ocanren { l == [] | fresh h, t in l == h :: t & h =/= e & not_mem e t };;

let rec distinct l =
  ocanren { l == [] | fresh h , t in l == h :: t & not_mem h t & distinct t };;

(** Find expr (sum only) that normalizes to 3/2 *)
  let open Inj in
  List.iter (fun fr -> print_string @@ (GT.show(frat)) fr; print_newline())
  @@ RStream.take ~n:10 @@ 
  run q (fun q -> ocanren { fresh a1, a2, a3, a4,
                            b1, b2, b3, b4 in
                            q == Sum (Sum (Sum (a1,b1),
                                           Sum (a2,b2)),
                                      Sum (Sum (a3,b3),
                                           Sum (a4,b4))) &
                            eval' q (Num (3,2))}) (fun q -> GRat.to_frat @@ project q)
*)



(*




(** Find expr (sum only) that normalizes to 1/3: this is a generate-and-test process *)
  let open Inj in
  List.iter (fun fr -> print_string @@ (GT.show(logic)) fr; print_newline())
  @@ RStream.take ~n:10 @@ 
run q (fun q -> ocanren { eval q (Num (1,3))}) (fun q -> q#reify(reify))

(** simplify 108 / 72 *)
let _ = 
  print_string @@ GT.show(ipl) @@ RStream.take ~n:10 @@
  run qr (fun q r -> ocanren { simplify 108 72 q r })
    (fun q r -> LNat.to_int @@ project q, LNat.to_int @@ project r);
  print_newline ();;





(** compute the gcd of 108 and 72 *)
let _ = 
  print_string @@ GT.show(intl) @@ RStream.take @@
  run q (fun q -> ocanren {gcd 108 72 q})  (fun q -> LNat.to_int @@ project q);
  print_newline ();;

(** remainder for 100/33 *)
let _ = 
  print_string @@ GT.show(intl) @@ RStream.take ~n:10 @@
  run q (fun q -> ocanren { remainder 100 33 q})
    (fun q -> LNat.to_int @@ project q);
  print_newline ();;

(** remainder for 100/44 *)
let _ = 
  print_string @@ GT.show(intl) @@ RStream.take ~n:10 @@
  run q (fun q -> ocanren { remainder 100 44 q})
    (fun q -> LNat.to_int @@ project q);
  print_newline ();;

(** Find q <= 100 and r such that q divides 100 with remainder r *)
let _ = 
  print_string @@ GT.show(ipl) @@ RStream.take ~n:50 @@
  run qr (fun q r -> ocanren { remainder 100 q r & LNat.( <= ) q 100 })
    (fun q r -> LNat.to_int @@ project q, LNat.to_int @@ project r);
  print_newline ();;


(** Enumerate pairs [(q,r)] where q is divisible by r *)  
let _ =
  print_string @@ GT.show(lnpl) @@ RStream.take ~n:10 @@
  run qr  (fun q r -> ocanren { divisible_by q r } )
    (fun q r -> q#reify(LNat.reify), r#reify(LNat.reify)) ;
  print_newline ();; 

(** Enumerate pairs [(q,r)] where q is divisible by r, discarding the
    cases where [q = 0] and where [q = r]. *)  
let _ =
  print_string @@ GT.show(ipl) @@ List.map (fun (a,b) -> LNat.to_int a, LNat.to_int b)
   @@ List.map LoRat.Prj.logic_to_ground @@ List.tl @@ List.tl @@ RStream.take ~n:100 @@
  run qr  (fun q r -> ocanren { divisible_by q r } )
    (fun q r -> q#reify(LNat.reify), r#reify(LNat.reify)) ;
  print_newline ();; 

(** Find all divisors of 15 *)
let _ =
  print_string @@ GT.show(intl) @@ RStream.take @@
  run q (fun q -> ocanren { divisible_by 15 q } )
    (fun q -> LNat.to_int @@ project q);
  print_newline ();;

(** Find all divisors of 97 (a prime number) *)
let _ =
  print_string @@ GT.show(intl) @@ RStream.take @@
  run q (fun q -> ocanren { divisible_by 97 q } )
    (fun q -> LNat.to_int @@ project q);
  print_newline ();;

(** Find all divisors of 85 *)
let _ =
  print_string @@ GT.show(intl) @@ RStream.take @@
  run q (fun q -> ocanren { divisible_by 85 q } )
    (fun q -> LNat.to_int @@ project q);
  print_newline ();;

(** Find all divisors of 80 *)
let _ =
  print_string @@ GT.show(intl) @@ RStream.take @@
  run q (fun q -> ocanren { divisible_by 80 q } )
    (fun q -> LNat.to_int @@ project q);
  print_newline ();;

(** Enumerate natural numbers divisible by 3 *)
let _ =
  print_string @@ GT.show(intl) @@ RStream.take ~n:10 @@
  run q (fun q -> ocanren { divisible_by q 3 } )
    (fun q -> LNat.to_int @@ project q);
  print_newline ();;

(** Evaluate an expression *)
let _ =
  print_string @@ GT.show(frat) @@ GRat.to_frat @@
  GRat.eval @@ GRat.of_frat @@
  Subt (Num (100,100), Prod (Num (3,5), Sum (Sum (Num (3,21), Num (12,14)), Num (3,9))));
  print_newline ();;

(** Evaluate an expression *)
let _ =
  print_string @@ GT.show(frat) @@ GRat.to_frat @@
  GRat.eval @@ GRat.of_frat (Sum (Sum (Num (3,21), Num (12,14)), Num (3,9)));
  print_newline ();;

(** Evaluate an expression *)
let _ =
  print_string @@ GT.show(frat) @@ GRat.to_frat @@
  GRat.eval @@ GRat.of_frat (Num (3,21));
  print_newline ();;

(** compute 4 plus 4 *)
let _ =
  print_string @@ GT.show(GT.int) @@ LNat.to_int @@
  GNat.( + ) (LNat.of_int 4) (LNat.of_int 4);
  print_newline ();;

(** compute 4 times 4 *)
let _ =
  print_string @@ GT.show(GT.int) @@ LNat.to_int @@
  GNat.( * ) (LNat.of_int 4) (LNat.of_int 4);
  print_newline ();;

(** compute 100 plus 4 *)
let _ =
  print_string @@ GT.show(GT.int) @@ LNat.to_int @@
  GNat.( + ) (LNat.of_int 100) (LNat.of_int 4);
  print_newline ();;

(** compute 100 times 4 *)
let _ =
  print_string @@ GT.show(GT.int) @@ LNat.to_int @@
  GNat.( * ) (LNat.of_int 100) (LNat.of_int 4);
  print_newline ();;

(** Enumerate all naturals less than or equals 10 *)
let _ =
  let open LNat in 
  print_string @@ GT.show(intl) @@  RStream.take @@
  run q (fun q -> ocanren { q <= 10 } ) (fun x -> to_int @@ project x);
  print_newline ();;

(** Test if 5 = 4 *)
let _ =
  print_string @@ GT.show(GT.bool) @@ GNat.( = ) (LNat.of_int 5) (LNat.of_int 4);
   print_newline ();;

(** Test if 5 = 5 *)
let _ =
  print_string @@ GT.show(GT.bool) @@ GNat.( = ) (LNat.of_int 5) (LNat.of_int 5);
   print_newline ();;

(** Test if 4 = 5 *)
let _ =
  print_string @@ GT.show(GT.bool) @@ GNat.( = ) (LNat.of_int 4) (LNat.of_int 5);
   print_newline ();;

(** Test if 5 < 4 *)
let _ =
  print_string @@ GT.show(GT.bool) @@ GNat.( < ) (LNat.of_int 5) (LNat.of_int 4);
   print_newline ();;

(** Test if 5 < 5 *)
let _ =
  print_string @@ GT.show(GT.bool) @@ GNat.( < ) (LNat.of_int 5) (LNat.of_int 5);
   print_newline ();;

(** Test if 4 < 5 *)
let _ =
  print_string @@ GT.show(GT.bool) @@ GNat.( < ) (LNat.of_int 4) (LNat.of_int 5);
  print_newline ();;

(** Test if 5 <= 4 *)
let _ =
  print_string @@ GT.show(GT.bool) @@ GNat.( <= ) (LNat.of_int 5) (LNat.of_int 4);
   print_newline ();;

(** Test if 5 <= 5 *)
let _ =
  print_string @@ GT.show(GT.bool) @@ GNat.( <= ) (LNat.of_int 5) (LNat.of_int 5);
   print_newline ();;

(** Test if 4 <= 5 *)
let _ =
  print_string @@ GT.show(GT.bool) @@ GNat.( <= ) (LNat.of_int 4) (LNat.of_int 5);
   print_newline ();;

(** compute 4 -5 *)
let _ =
  print_string @@
  (try
    GT.show(GT.int) @@ LNat.to_int @@
    GNat.( - ) (LNat.of_int 4) (LNat.of_int 5)
  with Invalid_argument s -> s );
    print_newline ();;

(** compute 4 -4 *)
let _ =
  print_string @@ GT.show(GT.int) @@ LNat.to_int @@
  GNat.( - ) (LNat.of_int 4) (LNat.of_int 4);
  print_newline ();;

(** compute 4 - 2 *)
let _ =
  print_string @@ GT.show(GT.int) @@ LNat.to_int @@
  GNat.( - ) (LNat.of_int 4) (LNat.of_int 2);
  print_newline ();;

(** compute 4 / 2 *)
let _ =
  print_string @@ GT.show(pr) @@ 
  (match GNat.( / ) (LNat.of_int 4) (LNat.of_int 2)
   with a,b -> LNat.to_int a, LNat.to_int b);
  print_newline ();;

(** compute 2 / 4 *)
let _ =
  print_string @@ GT.show(pr) @@ 
  (match GNat.( / ) (LNat.of_int 2) (LNat.of_int 4)
   with a,b -> LNat.to_int a, LNat.to_int b);
  print_newline ();;

(** compute 10 / 7 *)
let _ =
  print_string @@ GT.show(pr) @@ 
  (match GNat.( / ) (LNat.of_int 10) (LNat.of_int 7)
   with a,b -> LNat.to_int a, LNat.to_int b);
  print_newline ();;

(** compute 77 / 5 *)
let _ =
  print_string @@ GT.show(pr) @@ 
  (match GNat.( / ) (LNat.of_int 77) (LNat.of_int 5)
   with a,b -> LNat.to_int a, LNat.to_int b);
  print_newline ();;

(** comute 4 / 0 *)
let _ =
  print_string @@
  ( try
  GT.show(pr) @@ 
  match GNat.( / ) (LNat.of_int 4) (LNat.of_int 0)
  with a,b -> LNat.to_int a, LNat.to_int b with Division_by_zero -> "Division_by_zero");
  print_newline ();;

(** Find gcd *)

let _ =
  print_string @@ GT.show(GT.int) @@ LNat.to_int @@
  GNat.gcd (LNat.of_int 144) (LNat.of_int 55);
  print_newline ();;

let _ =
  print_string @@ GT.show(GT.int) @@ LNat.to_int @@
  GNat.gcd (LNat.of_int 144) (LNat.of_int 56);
  print_newline ();;

let _ =
  print_string @@ GT.show(GT.int) @@ LNat.to_int @@
  GNat.gcd (LNat.of_int 144) (LNat.of_int 57);
  print_newline ();;

let _ =
  print_string @@ GT.show(GT.int) @@ LNat.to_int @@
  GNat.gcd (LNat.of_int 144) (LNat.of_int 58);
  print_newline ();;

let _ =
  print_string @@ GT.show(GT.int) @@ LNat.to_int @@
  GNat.gcd (LNat.of_int 144) (LNat.of_int 59);
  print_newline ();;

(** Simplify ratios *)

let _ =
  print_string @@ GT.show(pr) @@ 
  (match GRat.simplify ((LNat.of_int 2), (LNat.of_int 4))
   with a,b -> LNat.to_int a, LNat.to_int b);
  print_newline ();;

let _ =
  print_string @@ GT.show(pr) @@ 
  (match GRat.simplify ((LNat.of_int 18800), (LNat.of_int 1000))
   with a,b -> LNat.to_int a, LNat.to_int b);
  print_newline ();;

(** Why we need to prefix [int] , [bool] and [show] with [GT] ? 
      Because, e.g,  the [int] as a parameter of [show], as in [show(int)], is 
      not a type expression but an object named [int] which is defined in [GT]. *)

*)

