(** This file tests flowchart sythesis with naive interpreters  *)
open OCanren;;
module L = List ;;
open OCanren.Std;;
open Coar;;
open Syntax;;
open Interp;;
open Interp.NoLet;;
open Twobit;;

let _ = Printf.printf "\n\n Test A \n\n";;

module TestA = struct
  let prog = ocanren {Brh(Var "x", Brh(Arr("y", Var "x"), Con c1, Con c2), Con c0)};;
  (* if x then if y[x] then 01 else 10 fi else 00 fi *)
  
  (** compute some/all input-output combinations, viz., IO pairs,  *)
  let specs : Spec.logic GT.list =  
    Stream.take  ~n: 100 @@ (* the answer contains associations for irrelevant variable names *)
    run q (fun q -> ocanren {fresh sts,res in
       eval_imp sts prog res 
       & q == (sts, res)}) (fun q -> q#reify(Spec.reify));;

  (** Print the IO pairs to synthesize against *)
  let _ =
    L.iter (fun x -> print_string @@ GT.show(Spec.logic) x;Printf.printf "\n\n") specs;;
(*
  (** count the number of the IO pairs *)
  let _ = Printf.printf "Synthesizing from %d input-output pairs...\n" (L.length specs) ;;

  (** convert the IO pairs from ground to groundi *)
  let specsi : Specs.groundi = Specs.grd2ijd specs;;

  (** synthesize a flowchart program that satisfies the IO pairs *) 
  let _ =
    L.iter (fun x -> print_string @@ GT.show(Signal.logic) x;print_newline())
    @@ Stream.take ~n:3200 (* record high: 3200 *)
    @@ run q (fun q -> ocanren {syn specsi q}) (fun q -> q#reify(Signal.reify)) 
*)

end;;

let _ = Printf.printf "\n\n Test B \n\n";;


module TestB = struct
  let prog = ocanren{
      Brh(Var "x", Brh(Arr("y", Var "x"), Arr("y", Var "x"), Var "x"), Arr("y", Con c0))
    };;
  
  (** compute some/all IO pairs *)
  let specs : Spec.logic GT.list =  
    Stream.take ~n:100 @@ 
    run q (fun q -> ocanren {fresh sts,res in
       eval_imp sts prog res
       & q == (sts, res)}) (fun q -> q#reify(Spec.reify));;

  (** Print the IO pairs to synthesize against *)
  let _ =
    L.iter (fun x -> print_string @@ GT.show(Spec.logic) x;Printf.printf "\n\n") specs;;
(*
  (** count the number of the IO pairs *)
  let _ = Printf.printf "Synthesizing from %d input-output pairs...\n" (L.length specs) ;;

  (** convert the IO pairs from ground to groundi *)
  let specsi : Specs.groundi = Specs.grd2ijd specs;;

  (** synthesize a flowchart program that satisfies the IO pairs *) 
  let _ =
    L.iter (fun x -> print_string @@ GT.show(Signal.logic) x;print_newline())
    @@ Stream.take ~n:40 @@ (* record high: 40 *)
    run q (fun q -> ocanren {syn specsi q}) (fun q -> q#reify(Signal.reify))
*)
end;;






