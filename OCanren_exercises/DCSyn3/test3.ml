(** This file tests the constant and array generators and ... *)

open OCanren;;
module L = List ;;
open OCanren.Std;;
open Dcsyn3;;
open Dcsyn3.InterpB;;
open Dcsyn3.InterpB.NoLet;;


@type strl = GT.string List.ground with show;;

(* xappendo *)
let _ =
  L.iter (fun x -> print_string @@  GT.show(strl) x;print_newline())
  @@ Stream.take ~n:10 @@ 
  run q (fun q -> ocanren {List.xappendo ["a";"x";"b";"y"] ["x";"y";"z"] q}) project

(* free_var *)
let _ =
  L.iter (fun x -> print_string @@  GT.show(strl) x;print_newline())
  @@ Stream.take ~n:10 @@ 
  run q (fun q -> ocanren {Expr.free_var
                             (Brh (Arr("x",Var "y"),
                                   Brh(Con c1,Var "y", Arr("x",Var "z")),
                                   Arr("x",Var "x"))) q}) project

let _ =
  L.iter (fun x -> print_string @@  GT.show(Expr.logic) x;print_newline())
  @@ Stream.take ~n:100 @@ 
  run q (fun q -> ocanren {Expr.free_var q ["x";"y";"z"]}) (fun q -> q#reify(Expr.reify))



(* xinsert *)

let _ =
  L.iter (fun x -> print_string @@  GT.show(strl) x;print_newline())
  @@ Stream.take ~n:10 @@ 
  run q (fun q -> ocanren {List.xinserto "hello" [] q}) project

let _ =
  L.iter (fun x -> print_string @@  GT.show(strl) x;print_newline())
  @@ Stream.take ~n:10 @@ 
  run q (fun q -> ocanren {List.xinserto "x" ["x";"y";"z"] q}) project

let _ =
  L.iter (fun x -> print_string @@  GT.show(strl) x;print_newline())
  @@ Stream.take ~n:10 @@ 
  run q (fun q -> ocanren {List.xinserto "y" ["x";"y";"z"] q}) project

let _ =
  L.iter (fun x -> print_string @@  GT.show(strl) x;print_newline())
  @@ Stream.take ~n:10 @@ 
  run q (fun q -> ocanren {List.xinserto "z" ["x";"y";"z"] q}) project

let _ =
  L.iter (fun x -> print_string @@  GT.show(strl) x;print_newline())
  @@ Stream.take ~n:10 @@ 
  run q (fun q -> ocanren {List.xinserto "hello" ["x";"y";"z"] q}) project

let _ =
  L.iter (fun x -> print_string @@  GT.show(strl) x;print_newline())
  @@ Stream.take ~n:10 @@ 
  run q (fun q -> ocanren {List.xinserto "z" ["x";"x";"z"] q}) project

(* Bool.tog *)
let _ =
  L.iter (fun x -> print_string @@ GT.show(Bool.ground) x;print_newline())
  @@ Stream.take ~n:3 @@ 
  run q (fun q -> ocanren {Bool.tog q}) project

(* Constnt2.tog *)
let _ =
  L.iter (fun x -> print_string @@ GT.show(Constnt2.ground) x;print_newline())
  @@ Stream.take ~n:5 @@ 
  run q (fun q -> ocanren {Constnt2.tog q}) project

(* Constnt3.tog *)
let _ =
  L.iter (fun x -> print_string @@ GT.show(Constnt3.ground) x;print_newline())
  @@ Stream.take ~n:9 @@ 
  run q (fun q -> ocanren {Constnt3.tog q}) project

(* Constant.tog *)
let _ =
  L.iter (fun x -> print_string @@ GT.show(Constant.ground) x;print_newline())
  @@ Stream.take ~n:17 @@ 
  run q (fun q -> ocanren {Constant.tog q}) project

(* Arr2.tog *)
let _ =
  L.iter (fun x -> print_string @@ GT.show(Arr2.ground) x;print_newline())
  @@ Stream.take @@ 
  run q (fun q -> ocanren {Arr2.tog q}) project

(* Arr4.tog *)
let _ =
  L.iter (fun x -> print_string @@ GT.show(Arr4.ground) x;print_newline())
  @@ Stream.take ~n:100 @@ 
  run q (fun q -> ocanren {Arr4.tog q}) project

(* Arr8.tog *)
let _ =
  L.iter (fun x -> print_string @@ GT.show(Arr8.ground) x;print_newline())
  @@ Stream.take (*~n:100*) @@ 
  run q (fun q -> ocanren {Arr8.tog q}) project

(* Arr16.tog *)
let _ =
  L.iter (fun x -> print_string @@ GT.show(Arr16.ground) x;print_newline())
  @@ Stream.take (*~n:100*) @@ 
  run q (fun q -> ocanren {Arr16.tog q}) project





