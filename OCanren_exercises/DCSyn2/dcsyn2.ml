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

(** Essentially a statement is a list of basic statements. 
    Note the similarity with OCanren.Std.List.t *)
@type ('bstat, 'self) stat = Nil | Seq of 'bstat * 'self
 with show, gmap, eq, compare;;

module Stat = struct
  @type ('a, 'b) t = ('a, 'b) stat with show, gmap, eq, compare;;
  let fmap = fun f1 f2 x -> GT.gmap(t) f1 f2 x;;
end;;

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
  module FBStat = Fmap(BStat);;
  module FStat  = Fmap2(Stat);;
  module FGraph = Fmap2(Graph);;

  let low  = fun () -> inj @@ FExpr.distrib Low;;
  let high = fun () -> inj @@ FExpr.distrib High;;
  let var  = fun x  -> inj @@ FExpr.distrib (Var x);;
  
  let ifte = fun x y z -> inj @@ FBStat.distrib (Ifte (x,y,z));;
  let asgn = fun x y   -> inj @@ FBStat.distrib (Asgn (x,y));;
  let skip = fun ()    -> inj @@ FBStat.distrib Skip;;

  let nil  = fun ()  -> inj @@ FStat.distrib Nil;;
  let seq  = fun x y -> inj @@ FStat.distrib (Seq (x,y));; 
  
  let expr = fun x     -> inj @@ FGraph.distrib (Expr x);;
  let lein = fun x y z -> inj @@ FGraph.distrib (Lein (x,y,z));;
  let mux  = fun x y z -> inj @@ FGraph.distrib (Mux (x,y,z));;
  let null = fun ()    -> inj @@ FGraph.distrib Null;;
end;;

open Inj;;

let rec translate sta gra =
  ocanren {
    prog == [] & grap == Null
  |
   {fresh s in prog == [s] &
               {{fresh e, s1, s2 in
                   s == Ifte (e, s1, s2)
                   & fresh g1, g2 in
                       grap == Mux (Expr e, g1, g2)
                       & translate [s1] g1
                       & translate [s2] g2 }
               |
                 {fresh v,e in
                   s == Asgn (Var v, e)
                   & grap == Lein(Var v, Expr e, Null) }}}
 |
  {fresh s, s', p in prog == s :: s' :: p &
                  {{fresh e, s1, s2 in
                   s == Ifte (e, s1, s2)
                   & fresh g1, g2 in
                       grap == Mux (Expr e, g1, g2)
                       & translate (s1 :: s' :: p) g1
                       & translate (s2 :: s' :: p) g2 }
               |
                 {fresh v, e, g in
                   s == Asgn (Var v, e)
                   & grap == Lein(Var v, Expr e, g)
                   & translate (s' :: p) g}}}
  };;


@type gra = (GT.string Expr.t, gra) Graph.t with show;;
@type sta = (GT.string Expr.t, sta) Stat.t with show;;
@type stal = sta GT.list with show;; 
@type pr = stal * gra with show;;

(* from impar to flowchar  *)
let _ =
  L.iter (fun x -> print_string @@ GT.show(gra) x ; print_newline()) @@  Stream.take ~n:2 @@
run q (fun q -> ocanren {translate [Asgn(Var "x", Var "y")] q}) project;;

(* from flowchar to impar  *)
let _ =
  L.iter (fun x -> print_string @@ GT.show(stal) x ; print_newline()) @@  Stream.take ~n:2 @@
  run q (fun q -> ocanren {translate q (Lein (Var ("x"), Expr (Var ("y")), Null))})
   (fun x -> List.to_list id @@ project x);;

(* from impar to flowchar  *)
let _ =
  L.iter (fun x -> print_string @@ GT.show(gra) x ; print_newline()) @@  Stream.take ~n:2 @@
  run q (fun q -> ocanren {translate [Asgn(Var "x", High);
                                      Ifte(Var "x",
                                           Asgn(Var "y", High),
                                           Asgn(Var "y", Low))] q}) project;;
(* from flowchar to impar  *)
let _ =
  L.iter (fun x -> print_string @@ GT.show(stal) x ; print_newline()) @@  Stream.take ~n:2 @@
  run q (fun q -> ocanren {translate q (Lein (Var ("x"),
                                              Expr (High),
                                              Mux (Expr (Var ("x")),
                                                   Lein (Var ("y"),
                                                         Expr (High), Null),
                                                   Lein (Var ("y"),
                                                         Expr (Low), Null))))})
     (fun x -> List.to_list id @@ project x);;

