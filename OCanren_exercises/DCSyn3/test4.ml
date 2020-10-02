(** This file tests flowchart sythesis *)
open OCanren;;
module L = List ;;
open OCanren.Std;;
open Dcsyn3;;
open Interp;;
open Interp.NoLet;;
open TwoBit;;


module TestA = struct
  (** compute all possible input/output combinations, viz., I/O pairs, of the given imperative program, 
      but take the first [n] of them; the default is to take all 1024 *)
  let specs : Spec.ground GT.list =  
    Stream.take ~n:41  @@ 
    run q (fun q -> ocanren {fresh prog, vs, sts,res in
       prog == Brh(Var "x", Brh(Arr("y", Var "x"), Con c1, Con c2), Con c0)
       & Expr.free_var prog vs
       & Expr.var_state vs sts
       & eval_imp sts prog res
       & q == (sts, res)}) project;;

  (** Print the I/O pairs *)
  let _ =
    L.iter (fun x -> print_string @@ GT.show(Spec.ground) x;print_newline()) specs;;

  (** count the number of the I/O pairs *)
  let _ = Printf.printf "Synthesizing from %d input-output pairs...\n" (L.length specs) ;;

  (** convert the I/O pairs from ground to groundi *)
  let specsi : Specs.groundi = Specs.grd2ijd specs;;

  (** synthesize a flowchart program that satisfies the I/O pairs *) 
  let _ =
    L.iter (fun x -> print_string @@ GT.show(Signal.logic) x;print_newline())
    @@ Stream.take ~n:1 @@
    run q (fun q -> ocanren {syn specsi q}) (fun q -> q#reify(Signal.reify))

(*
The result of synthesizing from 41 input-output pairs:

Mux (Port ("x"), Mux (Slice (Port ("y"), Src ((I, O))), Src ((O, I)), Src ((I, O))), Src ((O, O)))

The structure is already very similar to the imperative program, and the only difference is the index of [Slice].
 
The 42ed I/O pair is:
 
([("x", Conv ((I, I))); ("y", Arrv ((((O, O), (O, O)), ((O, O), (I, O)))))], Conv ((O, I)))

which causes the killed process. This means that the search was along a wrong direction.  
*)

end;;

module TestB = struct
  (** compute all possible input/output combinations, viz., I/O pairs, of the given imperative program, 
      but take the first [n] of them; the default is to take all *)
  let specs : Spec.ground GT.list =  
    Stream.take ~n:8  @@ 
    run q (fun q -> ocanren {fresh prog, vs, sts,res in
       prog == Brh(Var "x", Brh(Arr("y", Var "x"), Arr("y", Var "x"), Var "x"), Arr("y", Con c0))
       & Expr.free_var prog vs
       & Expr.var_state vs sts
       & eval_imp sts prog res
       & q == (sts, res)}) project;;

  (** Print the I/O pairs *)
  let _ =
    L.iter (fun x -> print_string @@ GT.show(Spec.ground) x;print_newline()) specs;;

  (** count the number of the I/O pairs *)
  let _ = Printf.printf "Synthesizing from %d input-output pairs...\n" (L.length specs) ;;

  (** convert the I/O pairs from ground to groundi *)
  let specsi : Specs.groundi = Specs.grd2ijd specs;;

  (** synthesize a flowchart program that satisfies the I/O pairs *) 
  let _ =
    L.iter (fun x -> print_string @@ GT.show(Signal.logic) x;print_newline())
    @@ Stream.take ~n:1 @@
    run q (fun q -> ocanren {syn specsi q}) (fun q -> q#reify(Signal.reify))

  (*  For n = 8, the synthesizer can give very involved answer *)
end;;

