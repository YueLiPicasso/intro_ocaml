(** A relational translator *)

open OCanren;;
module L = List;; (** alias of OCaml stdlib.List *)
open OCanren.Std;;

(** {0 Abstract types for syntactic categries} *)

(** shared syntactic categories *)
@type 'a expr = Low | High | Var of 'a
 with show, gmap, eq, compare;; (** use string as ['a] *)

module Expr = struct
  @type 'a t = 'a expr with show, gmap, eq, compare;;
  let fmap = fun f x -> GT.gmap(t) f x;;
end;;

(** syntactic categories unique to the imperative language *)

(** basic statement *)
@type ('expr, 'stat ) bstat =
     Ifte of 'expr * 'stat * 'stat (** if then else *)
   | Asgn of 'expr * 'expr         (** assignment *)
   | Skip           
 with show, gmap, eq, compare;;

module BStat = struct
  @type ('a,'b) t = ('a,'b) bstat  with show, gmap, eq, compare;;
  let fmap = fun f1 f2 x -> GT.gmap(t) f1 f2 x;;
end;;

(** The statement type [stat] is an [OCanren.Std.List] of [bstat]. *)

(** syntactic categories unique to the flowchart language *)
@type ('expr, 'self) graph = Expr of 'expr
                           | Lein of 'expr * 'self * 'self  (** let in *)
                           | Mux of 'self * 'self * 'self
                           | Null
 with show, gmap, eq, compare;;

module Graph = struct
  @type ('a,'b) t = ('a,'b) graph  with show, gmap, eq, compare;;
  let fmap = fun f1 f2 x -> GT.gmap(t) f1 f2 x;;
end;;

(** Injection primitives *)  
module Inj = struct
  module FExpr  = Fmap(Expr);;
  module FBStat = Fmap2(BStat);;
  module FGraph = Fmap2(Graph);;

  let low  = fun () -> inj @@ FExpr.distrib Low;;
  let high = fun () -> inj @@ FExpr.distrib High;;
  let var  = fun x  -> inj @@ FExpr.distrib (Var x);;
  
  let ifte = fun x y z -> inj @@ FBStat.distrib (Ifte (x,y,z));;
  let asgn = fun x y   -> inj @@ FBStat.distrib (Asgn (x,y));;
  let skip = fun ()    -> inj @@ FBStat.distrib Skip;;

  let expr = fun x     -> inj @@ FGraph.distrib (Expr x);;
  let lein = fun x y z -> inj @@ FGraph.distrib (Lein (x,y,z));;
  let mux  = fun x y z -> inj @@ FGraph.distrib (Mux (x,y,z));;
  let null = fun ()    -> inj @@ FGraph.distrib Null;;
end;;



open Inj;;

let rec translate sta gra =
  ocanren {
    sta == [] & gra == Null
 |
    fresh h, t in sta == h :: t &
      { { fresh e, s1, s2, s1', s2', g1, g2 in
          h == Ifte (e, s1, s2)
          & gra == Mux (Expr e, g1, g2)
          & List.appendo s1 t s1'
          & List.appendo s2 t s2'
          & translate s1' g1
          & translate s2' g2 }
       |
        { fresh v, e, g in
          h == Asgn (Var v, e)
          & gra == Lein (Var v, Expr e, g)
          & translate t g }
       |  h == Skip & translate t gra }
  };;



@type gra = (GT.string Expr.t, gra) Graph.t with show;;
@type sta = (GT.string Expr.t, sta) BStat.t List.ground with show;;


(* from impar to flowchar  *)
let _ =
  L.iter (fun x -> print_string @@ GT.show(gra) x ; print_newline()) @@  Stream.take ~n:2 @@
  run q (fun q -> ocanren {translate [Asgn(Var "x", Var "y")] q}) project;;

let _ = Printf.printf "\n\n\n";;

(* from flowchar to impar  *)
let _ =
  L.iter (fun x -> print_string @@ GT.show(sta) x ; print_newline()) @@  Stream.take ~n:6 @@
  run q (fun q -> ocanren {translate q (Lein (Var ("x"), Expr (Var ("y")), Null))}) project;;


let _ = Printf.printf "\n\n\n";;

(* from impar to flowchar  *)
let _ =
  L.iter (fun x -> print_string @@ GT.show(gra) x ; print_newline()) @@  Stream.take ~n:2 @@
  run q (fun q -> ocanren {translate [Asgn(Var "x", High);
                                      Ifte(Var "x",
                                           [Asgn(Var "y", High)],
                                           [Asgn(Var "y", Low)])] q}) project;;

let _ = Printf.printf "\n\n\n";;

(* from flowchar to impar  *)
let _ =
  L.iter (fun x -> print_string @@ GT.show(sta) x ; print_newline()) @@  Stream.take ~n:2 @@
  run q (fun q -> ocanren {translate q (Lein (Var ("x"),
                                              Expr (High),
                                              Mux (Expr (Var ("x")),
                                                   Lein (Var ("y"),
                                                         Expr (High), Null),
                                                   Lein (Var ("y"),
                                                         Expr (Low), Null))))})
      project;;


let _ = Printf.printf "\n\n\n";;

(* from impar to flowchar  *)
let _ =
  L.iter (fun x -> print_string @@ GT.show(gra) x ; print_newline()) @@  Stream.take ~n:2 @@
  run q (fun q -> ocanren {translate [Asgn(Var "x", Low);
                                      Ifte(Var "x",
                                           [Skip],
                                           [Asgn(Var "y", High);
                                            Ifte(Var "y",
                                                 [Asgn(Var "x", High);
                                                  Asgn(Var "y", Low)],
                                                 [Skip])])] q}) project;;

let _ = Printf.printf "\n\n\n";;

(* from flowchar to impar  *)
let _ =
  L.iter (fun x -> print_newline();print_string @@ GT.show(sta) x ; print_newline()) @@  Stream.take ~n:7 @@
  run q (fun q ->
      ocanren {translate  q
                 (Lein (Var ("x"), Expr (Low),
                        Mux (Expr (Var ("x")),
                             Null,
                             Lein (Var ("y"), Expr (High),
                                   Mux (Expr (Var ("y")),
                                        Lein (Var ("x"), Expr (High),
                                              Lein (Var ("y"), Expr (Low),
                                                    Null)),
                                        Null)))))})
      project;;
