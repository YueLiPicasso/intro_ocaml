(** This file tests interpreters with bound *)
open OCanren;;
module L = Stdlib.List;;
open OCanren.Std;;
open Coar;;
open Dcsyn3;;
open InterpSZ;;
open InterpSZ.NoLet;;
open TwoBit;;


module Test3 = struct
  (** given an imperative program *)
  let prog = ocanren { Brh(Var "x", Brh(Arr("y", Var "x"), Con c1, Con c2), Con c0) };;
  
  (** First, collect some or all I/O pairs of a given imperative program *)
  let specs : Spec.ground GT.list =  
    Stream.take ~n:88 @@ 
    run q (fun q -> ocanren {fresh vs, sts,res,sz in 
       Expr.free_var prog vs
       & Expr.var_state vs sts
       & eval_imp sts prog res sz
       & q == (sts, res)}) project;;

  (** Print the I/O pairs *)
  let _ =
    L.iter (fun x -> print_string @@ GT.show(Spec.ground) x;print_newline()) specs;;

  (** count the number of the I/O pairs *)
  let _ = Printf.printf "Synthesizing from %d input-output pairs...\n" (L.length specs) ;;

  (** Second, convert the I/O pairs list to groundi *)
  let specsi : Specs.groundi = Specs.grd2ijd specs;;

  (** Third, compute the size of the imperative program *)
  let sz = L.hd @@ Stream.take @@ run q (fun q -> Expr.size prog q) project;;

  let _  = Printf.printf "The imperative program has %d cnnstructors.\n " @@
    Nat.to_int sz;; 

  let szi = Nat.nat sz;;

  (** Now synthsize ... *)
  let _ =
    L.iter (fun x -> print_string @@ GT.show(Signal.logic) x;print_newline())
    @@ Stream.take ~n:1 @@ run q (fun q -> syn specsi q (Nat.succ szi))
      (fun q -> q#reify(Signal.reify));;

(* n = 88 is the max. The synthesized program is : 
Mux (Slice (Port ("y"), Port ("x")), 
Src ((O, I)), 
Mux (Port ("x"), Src ((I, O)), Src ((O, O))))
*)
end;;

(*
@type specsz = (Nat.ground, Spec.ground) Pair.ground with show;;

(** forward run of eval_imp *)
module Test1 = struct
  let specs =  
    Stream.take ~n:41  @@ 
    run q (fun q -> ocanren {fresh prog, vs, sts,res,sz in
       prog == Brh(Var "x", Brh(Arr("y", Var "x"), Con c1, Con c2), Con c0)
       & Expr.free_var prog vs
       & Expr.var_state vs sts
       & eval_imp sts prog res sz
       & q == (sz, sts, res)}) project;;
  
  let _ =
    L.iter (fun x -> print_string @@ GT.show(specsz) x;print_newline()) specs;;
end;;

(** forward run of eval_sig *)
module Test2 = struct
  let specs =  
    Stream.take ~n:41  @@ 
    run q (fun q -> ocanren {fresh prog, vs, sts,res,sz in
       prog == Mux(Port "x", Mux(Slice(Port "y", Port "x"), Src c1, Src c2), Src c0)
       & Signal.free_var prog vs
       & Signal.var_state vs sts
       & eval_sig sts prog res sz
       & q == (sz, sts, res)}) project;;
  
  let _ =
    L.iter (fun x -> print_string @@ GT.show(specsz) x;print_newline()) specs;;
end;;
*)
