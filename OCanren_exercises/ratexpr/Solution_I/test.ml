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

(** test [eval''']: quine: the first answer is not good; could be 
    corrected but does not worth it for now --- simply discrad it *)
let _ =
  let open Inj in 
  List.iter (fun fr -> print_string @@ (GT.show(logic)) fr; print_newline()) @@
  List.tl @@ RStream.take ~n:100 @@ 
  run q (fun q ->  ocanren { eval''' q q })
    (fun q -> q#reify(reify))
;;

(** test [eval''']: check : OK *)
let _ =
  let open Inj in 
  List.iter (fun fr -> print_string @@ (GT.show(logic)) fr; print_newline())
  @@ RStream.take ~n:1 @@ 
  run q (fun q ->  ocanren { eval'''
                               (Sum (Prod (Num (1, 1), Num (1, 1)),
                                     Sum (Num (1, 1), Num (2, 1))))
                               (Num (4,1)) })
    (fun q -> q#reify(reify))
;;


(** test [eval''']: forward: fast *)
let _ =
  let open Inj in 
  List.iter (fun fr -> print_string @@ (GT.show(frat)) fr; print_newline())
  @@ RStream.take ~n:1 @@ 
  run q (fun q ->  ocanren { eval'''
                               (Sum (Prod (Num (1, 1), Num (1, 1)),
                                     Sum (Num (1, 1), Num (2, 1))))
                               q })
    (fun q -> GRat.to_frat @@ project q)
;;

(** test [eval''']: backward, OK, efficient  *)
let _ =
  let open Inj in
  List.iter (fun fr -> print_string @@ (GT.show(logic)) fr; print_newline())
  @@ RStream.take ~n:100 @@ 
  run q (fun q ->  ocanren { eval''' q (Num  (4,1)) })
    (fun q -> q#reify(reify))
;;


(** test [eval''_a]: forward : OK *)
let _ =
  let open Inj in
  List.iter (fun fr -> print_string @@ (GT.show(frat)) fr; print_newline())
  @@ RStream.take ~n:1 @@ 
  run q (fun q ->
      ocanren { eval''_a 
                  (Sum  (Prod (Num (1, 2), Num (1,3)), Subt (Num (2, 1), Num (1,3))))
                  q }) (fun q -> GRat.to_frat @@ project q)
;;

(** test [eval''_a]: backward : OK *)
let _ =
  let open Inj in
  List.iter (fun fr -> print_string @@ (GT.show(logic)) fr; print_newline())
  @@ RStream.take ~n:10 @@ 
  run q (fun q ->
      ocanren { eval''_a q (Num (1,4))}) (fun q ->q#reify(reify))
;;

(** test [eval''_a]: quine : OK *)
let _ =
  let open Inj in
  List.iter (fun fr -> print_string @@ (GT.show(logic)) fr; print_newline())
  @@ RStream.take ~n:10 @@ 
  run q (fun q ->
      ocanren { eval''_a q q}) (fun q ->q#reify(reify))
;;

(** test [eval''_a]: the backward optimized branches do not seem to work here *)
let _ =
  let open Inj in
  List.iter (fun fr -> print_string @@ (GT.show(logic)) fr; print_newline())
  @@ RStream.take ~n:50 @@ 
  run q (fun q ->  ocanren { eval''_a q (Num  (4,1)) })
    (fun q -> q#reify(reify))
;;

(** test [eval''_a]: killed without answers  *)
let _ =
  let open Inj in
  List.iter (fun q,r ->
      print_string @@ (GT.show(frat)) q;
      print_string " = ";
      print_string @@ (GT.show(frat)) r;
      print_newline())
  @@ RStream.take ~n:20 @@ 
  run qr (fun q r ->  ocanren { eval''_a q (Num  (4,1)) & eval''_a q r })
    (fun q r -> GRat.to_frat @@ project q,
              GRat.to_frat @@ project r)
;;






(** test [eval'']: Find expr that normalizes to 5/3 : OK *)
let _ =
  let open Inj in
  List.iter (fun fr -> print_string @@ (GT.show(frat)) fr; print_newline())
  @@ RStream.take ~n:10 @@ 
  run q (fun q -> ocanren { eval'' q (Num (5,3))}) (fun q -> GRat.to_frat @@ project q)

;;

(** test [eval'']: Find some coprime numbers *)
let _ =
  List.iter (fun q ->
      print_string @@ (GT.show(frat)) q;
      print_newline())
  @@ RStream.take ~n:10 @@ 
  run q (fun q -> ocanren { eval'' q q}) (fun r -> GRat.to_frat @@ project r)

;;

(** test [eval'']: generate equations  *)
let _ =
  List.iter (fun q,r ->
      print_string @@ (GT.show(logic)) q;
      print_string " = ";
      print_string @@ (GT.show(frat)) r;
      print_newline())
  @@ RStream.take ~n:10 @@ 
  run qr (fun q r-> ocanren { eval'' q r})
    (fun q r -> q#reify(Inj.reify),
                GRat.to_frat @@ project r)

;;

(** test [eval'']: evaluate expr : OK *)
let _ =
  let open Inj in
  List.iter (fun fr -> print_string @@ (GT.show(frat)) fr; print_newline())
  @@ RStream.take ~n:1 @@ 
  run q (fun q ->
      ocanren { eval'' 
                  (Sum  (Prod (Num (1, 2), Num (1,3)), Subt (Num (2, 1), Num (1,3))))
                  q }) (fun q -> GRat.to_frat @@ project q)
;;


(** test [eval'']: evaluate expr OK *)
let _ =
  let open Inj in
  List.iter (fun fr -> print_string @@ (GT.show(frat)) fr; print_newline())
  @@ RStream.take ~n:1 @@ 
  run q (fun q ->
      ocanren { eval'' (Prod (Num (1, 2), Num (1,3)))
                  q }) (fun q -> GRat.to_frat @@ project q)
;;


(** test [eval'']: evaluate expr OK *)
let _ =
  let open Inj in
  List.iter (fun fr -> print_string @@ (GT.show(frat)) fr; print_newline())
  @@ RStream.take ~n:1 @@ 
  run q (fun q ->
      ocanren { eval'' (Prod (Num (1, 2), Num (1,4)))
                  q }) (fun q -> GRat.to_frat @@ project q)
;;


(** test [eval'']: evaluate expr OK *)
let _ =
  let open Inj in
  List.iter (fun fr -> print_string @@ (GT.show(frat)) fr; print_newline())
  @@ RStream.take ~n:1 @@ 
  run q (fun q ->
      ocanren { eval'' (Prod (Num (1, 7), Num (1,100)))
                  q }) (fun q -> GRat.to_frat @@ project q)
;;



(** test [eval'']: evaluate expr OK *)
let _ =
  let open Inj in
  List.iter (fun fr -> print_string @@ (GT.show(frat)) fr; print_newline())
  @@ RStream.take ~n:1 @@ 
  run q (fun q ->
      ocanren { eval''
                  (Sum (Num (1, 1), Sum (Num (1, 4), Prod (Num (1, 4), Num (1, 1)))))
                  q }) (fun q -> GRat.to_frat @@ project q)

;;


(** test [eval'']: evaluate expr: OK *)
let _ =
  let open Inj in
  List.iter (fun fr -> print_string @@ (GT.show(frat)) fr; print_newline())
  @@ RStream.take ~n:1 @@ 
  run q (fun q ->
      ocanren { eval'' (Subt (Num (1, 2), Num (1,3)))
                  q }) (fun q -> GRat.to_frat @@ project q)
;;


(** test [eval'']: evaluate expr: OK *)
let _ =
  let open Inj in
  List.iter (fun fr -> print_string @@ (GT.show(frat)) fr; print_newline())
  @@ RStream.take ~n:1 @@ 
  run q (fun q ->
      ocanren { eval'' (Num (280, 300))
                  q }) (fun q -> GRat.to_frat @@ project q)


(**test [eval'']:  evaluate expr: OK *)
let _ =
  let open Inj in
  List.iter (fun fr -> print_string @@ (GT.show(frat)) fr; print_newline())
  @@ RStream.take ~n:1 @@ 
  run q (fun q ->
      ocanren { eval'' (Sum (Num (2, 3), Num (1,3)))
                  q }) (fun q -> GRat.to_frat @@ project q)


(** test [eval'']: evaluate expr: OK *)
let _ =
  let open Inj in
  List.iter (fun fr -> print_string @@ (GT.show(frat)) fr; print_newline())
  @@ RStream.take ~n:1 @@ 
  run q (fun q ->
      ocanren { eval'' (Subt (Num (2, 3), Num (1,3)))
                  q }) (fun q -> GRat.to_frat @@ project q)


(** use [simplify''] to generate some co-prime numbers *)
let _ = 
  List.iter  (fun x -> print_string @@ GT.show(pr) x ;  print_newline())
  @@ RStream.take ~n:90 @@ let open LNat in
  run two (fun a b -> ocanren {a < b &  a =/= one & simplify'' a b a b })
    (fun a b  -> to_int @@ project a ,
                 to_int @@ project b )
;;

(** test [simplify'']: find the number to which 364 / 420  simplifies: fast *)
let _ = 
  print_string @@ GT.show(ipl) @@ RStream.take ~n:1 @@
  run qr (fun q r -> ocanren { simplify'' 364 420  q r })
    (fun q r -> LNat.to_int @@ project q, LNat.to_int @@ project r);
  print_newline ();;


(** test [simplify'']: find the number to which  420 / 364  simplifies: fast *)
let _ = 
  print_string @@ GT.show(ipl) @@ RStream.take ~n:1 @@
  run qr (fun q r -> ocanren { simplify'' 420 364  q r })
    (fun q r -> LNat.to_int @@ project q, LNat.to_int @@ project r);
  print_newline ();;


(**  test [simplify'']: find numbers that simplify to 3 / 2: quick for ~n:20, 
    OK for ~n:50, and very slow for ~n:100 *)
let _ = 
  print_string @@ GT.show(ipl) @@ RStream.take ~n:20 @@
  run qr (fun q r -> ocanren { simplify'' q r 3 2 })
    (fun q r -> LNat.to_int @@ project q, LNat.to_int @@ project r);
  print_newline ();;


(** test [simplify'']:  find numbers that simplify to 2 / 3: ok for ~n:50. *)
let _ = 
  print_string @@ GT.show(ipl) @@ RStream.take ~n:20 @@
  run qr (fun q r -> ocanren { simplify'' q r 2 3 })
    (fun q r -> LNat.to_int @@ project q, LNat.to_int @@ project r);
  print_newline ();;


(** local test for [simplify''], for the case all a b a' b' are free *)
let _ =
  let cvt = fun a,b,c,d -> a,b,c, LNat.to_int @@ LoNat.Prj.logic_to_ground d in
  print_string "  a           b    b'    q\n";
  List.iter  (fun x -> print_string @@ GT.show(lnp4') @@ cvt x ;  print_newline())
  @@ RStream.take ~n:500 @@ let open LNat in
  run four (fun a b b' q -> ocanren {b < a & ( * ) q b' b})
    (fun a b b' q -> a  # reify(reify),
                     b  # reify(reify),
                     b' # reify(reify),
                     q  # reify(reify))
;;

(** Use [gcd] to generate:  *)
let _ =
  let rec conv_to_g = function [] -> [] | (a,b,c) :: t ->
    begin
      try (LoNat.Prj.logic_to_ground a,
           LoNat.Prj.logic_to_ground b,
           LoNat.Prj.logic_to_ground c) ::  conv_to_g t
      with Not_a_value -> conv_to_g t
    end
  in
  let li = List.map (fun a,b,c -> LNat.to_int a, LNat.to_int b,LNat.to_int c) @@
    conv_to_g @@ RStream.take ~n:500 @@ let open LNat in
    run qrs (fun q r s -> gcd q r s)
      (fun q r s-> q#reify(LNat.reify),
                   r#reify(LNat.reify),
                   s#reify(LNat.reify))
  in let  compr = fun (a,b,c) (a',b',c') -> match compare b b' with
      | 0 -> compare a a'
      | c -> c
  in
  List.iter (fun x -> print_string @@ GT.show(pr3) x ; print_newline())
  @@ List.fast_sort compr li

;;

(** compute the [gcd'] of 72 and 108 *)
let _ = 
  print_string @@ GT.show(intl) @@ RStream.take @@
  run q (fun q -> ocanren {gcd' 72 108 q})  (fun q -> LNat.to_int @@ project q);
  print_newline ()

;;

(** Use [gcd'] generate again with an extra constraint: same as [gcd] exactly *)
let _ =
  let compr = fun (a,b,c) (a',b',c') -> match compare b b' with
    | 0 -> compare a a'
    | c -> c
  and  li = RStream.take ~n:1000 @@
    run qrs (fun q r s -> LNat.( < ) r q &&& LoNat.gcd' q r s )
      (fun q r s-> LNat.to_int @@ project q,
                   LNat.to_int @@ project r,
                   LNat.to_int @@ project s)
  in  List.iter (fun x -> print_string @@ GT.show(pr3) x ;  print_newline())
  @@ List.fast_sort compr li
    
 ;;


(** Use [gcd'] to generate: mant cases are missing, for example, when [r]
 was 2, [q] only range on even numbers after 10.   *)
let _ =
  let rec conv_to_g = function [] -> [] | (a,b,c) :: t ->
    begin
      try (LoNat.Prj.logic_to_ground a,
           LoNat.Prj.logic_to_ground b,
           LoNat.Prj.logic_to_ground c) ::  conv_to_g t
      with Not_a_value -> conv_to_g t
    end
  in
  let li = List.map (fun a,b,c -> LNat.to_int a, LNat.to_int b,LNat.to_int c) @@
    conv_to_g @@ RStream.take ~n:500 @@ let open LNat in
    run qrs (fun q r s -> gcd' q r s)
      (fun q r s-> q#reify(LNat.reify),
                   r#reify(LNat.reify),
                   s#reify(LNat.reify))
  in let  compr = fun (a,b,c) (a',b',c') -> match compare b b' with
      | 0 -> compare a a'
      | c -> c
  in
  List.iter (fun x -> print_string @@ GT.show(pr3) x ; print_newline())
  @@ List.fast_sort compr li
;;


(** Use [gcd'] to geneerate: the first two are generic cases: the gcd between zero 
    and a non-zero number is that non-zero number. Between two identical number the 
    gcd is any one of them. The rest answers are concrete. *)
let _ = 
  let li = RStream.take ~n:500 @@ let open LNat in
    run qrs (fun q r s -> gcd' q r s)
      (fun q r s-> q#reify(LNat.reify),
                   r#reify(LNat.reify),
                   s#reify(LNat.reify))
  in  List.iter (fun x -> print_string @@ GT.show(lnp3) x ; print_newline()) li
;;


(** test [simplify']: find numbers that simplify to 3 / 2 *)
let _ = 
  print_string @@ GT.show(ipl) @@ RStream.take ~n:3 @@
  run qr (fun q r -> ocanren { simplify' q r 3 2 })
    (fun q r -> LNat.to_int @@ project q, LNat.to_int @@ project r);
  print_newline ();;

(** find  numbers [q], [r] and [s] such that [gcd q r s] for some r < q. This mimics
    the internals of [simplify] when it is used backward. We could see that [q] grows
    much faster than [r]. We also guess the way [simplify] works backward is that
    it generates pairs of numbers together with their gcd, and checks if they simplify to
    the given number. This two combined, we say that when simplify is used backward to 
    find a small number of answers, it could work fast. But when  it is asked to find a large
    number of answers, due to the fact that [gcd] does not produce evenly distributed 
    answers, this would prolong the waiting time indefinitely. We could further sort the
    answers to see the relative speed of growth of [q] and [r].  We could see that in 
    the 1000 answers, when [r] stays at 1, [q] ranged from 2 to 240, and when [r] stayed 
    at 2, [q] ranged from 3 to 240; similar for [r] equals 3. When [r] stayed at 4, [q] grown
    to 188 from 5; when [r] is 5, [q] grown from 6 to 160; [r] 6, [q] 7 to 138; 
    [r] 7, [q] 8 to 112; [r] 8, [q] 9 to 72; the biggest [r] is 14 before the process was 
    killed by the system automatically. *)
let _ =
  let compr = fun (a,b,c) (a',b',c') -> match compare b b' with
    | 0 -> compare a a'
    | c -> c
  and  li = RStream.take ~n:1000 @@
    run qrs (fun q r s -> LNat.( < ) r q &&& LoNat.gcd q r s )
      (fun q r s-> LNat.to_int @@ project q,
                   LNat.to_int @@ project r,
                   LNat.to_int @@ project s)
  in  List.iter (fun x -> print_string @@ GT.show(pr3) x ;  print_newline())
  @@ List.fast_sort compr li
    
 ;;


(** test [eval]: evaluate expr *)
let _ =
  let open Inj in
  List.iter (fun fr -> print_string @@ (GT.show(frat)) fr; print_newline())
  @@ RStream.take ~n:500 @@ 
  run q (fun q ->
      ocanren { eval
                  (Sum (Num (1, 1), Sum (Num (1, 4), Prod (Num (1, 4), Num (1, 1)))))
                  q }) (fun q -> GRat.to_frat @@ project q)

;;


(** test [eval']: Find expr that normalizes to 3/2 *)
let _ =
  let open Inj in
  List.iter (fun fr -> print_string @@ (GT.show(frat)) fr; print_newline())
  @@ RStream.take ~n:50 @@ 
  run q (fun q -> ocanren { eval' q (Num (3,2))}) (fun q -> GRat.to_frat @@ project q)

;;


(** test [eval]: evaluate expr: this does  not work because non-commutativity of conjuncts in 
    [simplify'] makes it impossible to even guess the correct answer. *)
let _ =
  let open Inj in
  List.iter (fun fr -> print_string @@ (GT.show(frat)) fr; print_newline())
  @@ RStream.take ~n:500 @@ 
  run q (fun q ->
      ocanren { eval'
                  (Sum (Num (1, 1), Sum (Num (1, 4), Prod (Num (1, 4), Num (1, 1)))))
                  q }) (fun q -> GRat.to_frat @@ project q);;
;;

(** Test [simplify_4]: find numbers that simplify to 3 / 2 *)
let _ = 
  print_string @@ GT.show(ipl) @@ RStream.take ~n:5 @@
  run qr (fun q r -> ocanren { simplify_4 q r 3 2 })
    (fun q r -> LNat.to_int @@ project q, LNat.to_int @@ project r);
  print_newline ();;

(** Test [simplify_4]: find the number to which 364 / 420  simplifies *)
let _ = 
  print_string @@ GT.show(ipl) @@ RStream.take ~n:1 @@
  run qr (fun q r -> ocanren { simplify_4 364 420  q r })
    (fun q r -> LNat.to_int @@ project q, LNat.to_int @@ project r);
  print_newline ();;

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

(** Test [simplify']: find what  simplifies  to what. Most of the gernated tuples have form (0, x, 0, x).
This is cause by backtracking on the last goal *)
let _ = 
  print_string @@ GT.show(ipl4) @@ RStream.take ~n:400 @@
  run qrst (fun q r s t-> ocanren { simplify' q r s t})
    (fun q r s t -> LNat.to_int @@ project q,
                    LNat.to_int @@ project r,
                    LNat.to_int @@ project s,
                    LNat.to_int @@ project t);
  print_newline ();;


(** Test [simplify']: find numbers to which 300 / 100  simplifies *)
let _ = 
  print_string @@ GT.show(ipl) @@ RStream.take ~n:11 @@
  run qr (fun q r -> ocanren { simplify' 300 200  q r })
    (fun q r -> LNat.to_int @@ project q, LNat.to_int @@ project r);
  print_newline ();;


(** Test [simplify']: find numbers that simplify to 3 / 2 *)
let _ = 
  print_string @@ GT.show(ipl) @@ RStream.take ~n:11 @@
  run qr (fun q r -> ocanren { simplify' q r 3 2 })
    (fun q r -> LNat.to_int @@ project q, LNat.to_int @@ project r);
  print_newline ();;


(** test [eval]: forward run *)
let _ = let open Inj in
  print_string @@ GT.show(frat) @@ GRat.to_frat @@ List.hd @@ RStream.take ~n:1 @@
  run q (fun q -> ocanren {eval (Sum (Num (1, 3), Num (4, 5))) q})
    project;
  print_newline();;

(**  Test [simplify]: find bounded numbers that simplify to 3 / 2: 
    stupid generate and test *)
let _ =
  let max_int' = OCanren.Std.nat 10 in
  print_string @@ GT.show(ipl) @@ RStream.take ~n:11 @@
  run qr (fun q r -> ocanren { LNat.( < ) q max_int' & LNat.( < ) r max_int' & simplify q r 3 2 })
    (fun q r -> LNat.to_int @@ project q, LNat.to_int @@ project r);
  print_newline ();;


(** test [eval']: Find expr (sum only) that normalizes to 3/2 *)
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
;;


(** test [eval]: quine: OK *)
let _ =
  let open Inj in 
  List.iter (fun fr -> print_string @@ (GT.show(logic)) fr; print_newline()) @@
  List.tl @@ RStream.take ~n:100 @@ 
  run q (fun q ->  ocanren { eval q q })
    (fun q -> q#reify(reify))
;;


(** test [eval]: Find expr (sum only) that normalizes to 1/3: this is a generate-and-test process *)
  let open Inj in
  List.iter (fun fr -> print_string @@ (GT.show(logic)) fr; print_newline())
  @@ RStream.take ~n:10 @@ 
run q (fun q -> ocanren { eval q (Num (1,3))}) (fun q -> q#reify(reify))

(** test [simplify]: 108 / 72 *)
let _ = 
  print_string @@ GT.show(ipl) @@ RStream.take ~n:10 @@
  run qr (fun q r -> ocanren { simplify 108 72 q r })
    (fun q r -> LNat.to_int @@ project q, LNat.to_int @@ project r);
  print_newline ();;


(** test [gcd]: compute the gcd of 108 and 72 *)
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

