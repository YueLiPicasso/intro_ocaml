(** This file tests flowchart sythesis with naive interpreters  *)
open OCanren;;
module L = List ;;
open OCanren.Std;;
open Coar;;
open Syntax;;
open Interp;;
open Interp.NoLet;;
open Twobit;;


module TestA = struct
  let prog = ocanren {Brh(Var "x", Brh(Arr("y", Var "x"), Con c1, Con c2), Con c0)};;
  
  (** compute some/all input-output combinations, viz., IO pairs,  *)
  let specs : Spec.ground GT.list =  
    Stream.take ~n:41  @@ (* record high: 41 *)
    run q (fun q -> ocanren {fresh vs, sts,res in
       Expr.free_var prog vs
       & Expr.var_state vs sts
       & eval_imp sts prog res
       & q == (sts, res)}) project;;

  (** Print the IO pairs to synthesize against *)
  let _ =
    L.iter (fun x -> print_string @@ GT.show(Spec.ground) x;print_newline()) specs;;

  (** count the number of the IO pairs *)
  let _ = Printf.printf "Synthesizing from %d input-output pairs...\n" (L.length specs) ;;

  (** convert the IO pairs from ground to groundi *)
  let specsi : Specs.groundi = Specs.grd2ijd specs;;

  (** synthesize a flowchart program that satisfies the IO pairs *) 
  let _ =
    L.iter (fun x -> print_string @@ GT.show(Signal.logic) x;print_newline())
    @@ Stream.take ~n:3200 (* record high: 3200 *)
    @@ run q (fun q -> ocanren {syn specsi q}) (fun q -> q#reify(Signal.reify))

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
  let prog = ocanren{
      Brh(Var "x", Brh(Arr("y", Var "x"), Arr("y", Var "x"), Var "x"), Arr("y", Con c0))
    };;
  
  (** compute some/all IO pairs *)
  let specs : Spec.ground GT.list =  
    Stream.take ~n:216 @@ (* record high: 216 *)
    run q (fun q -> ocanren {fresh vs, sts,res in
       Expr.free_var prog vs
       & Expr.var_state vs sts
       & eval_imp sts prog res
       & q == (sts, res)}) project;;

  (** Print the IO pairs to synthesize against *)
  let _ =
    L.iter (fun x -> print_string @@ GT.show(Spec.ground) x;print_newline()) specs;;

  (** count the number of the IO pairs *)
  let _ = Printf.printf "Synthesizing from %d input-output pairs...\n" (L.length specs) ;;

  (** convert the IO pairs from ground to groundi *)
  let specsi : Specs.groundi = Specs.grd2ijd specs;;

  (** synthesize a flowchart program that satisfies the IO pairs *) 
  let _ =
    L.iter (fun x -> print_string @@ GT.show(Signal.logic) x;print_newline())
    @@ Stream.take ~n:40 @@ (* record high: 40 *)
    run q (fun q -> ocanren {syn specsi q}) (fun q -> q#reify(Signal.reify))

end;;


