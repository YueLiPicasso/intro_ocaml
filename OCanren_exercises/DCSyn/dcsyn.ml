(** A relational translator *)

open OCanren;;
module L = List;; (** alias of OCaml stdlib.List *)
open OCanren.Std;;

(** {0 Abstract types for syntactic categries} *)

(** shared syntactic categories *)
@type 'a expr = Low | High | Var of 'a with show, gmap, eq, compare;; (** use string as ['a] *)

module Expr = struct
  @type 'a t = 'a expr with show, gmap, eq, compare;;
  let fmap = fun f x -> GT.gmap(t) f x;;
end;;

(** syntactic categories unique to the imperative language *)
@type ('var, 'expr, 'stat ) stat = Ifte of 'expr * 'stat * 'stat (** if then else *)
                                 | Asgn of 'var * 'expr          (** assignment *)
 with show, gmap, eq, compare;;

module Stat = struct
  @type ('a,'b,'c) t = ('a,'b,'c) stat  with show, gmap, eq, compare;;
  let fmap = fun f1 f2 f3 x -> GT.gmap(t) f1 f2 f3 x;;
end;;

(** a {i program} is a logical list of logical [stat] *)

(** syntactic categories unique to the flowchart language *)
@type ('var, 'expr, 'self) graph = Expr of 'expr
                                 | Lein of 'var * 'self * 'self  (** let in *)
                                 | Mux of 'self * 'self * 'self
                                 | Null
 with show, gmap, eq, compare;;

module Graph = struct
  @type ('a,'b,'c) t = ('a,'b,'c) graph  with show, gmap, eq, compare;;
  let fmap = fun f1 f2 f3 x -> GT.gmap(t) f1 f2 f3 x;;
end;;

(** Injection and projection primitives *)  
module Xjection = struct
  module FExpr = Fmap(Expr);;
  module FStat = Fmap3(Stat);;
  module FGraph = Fmap3(Graph);;

  let low  = fun () -> inj @@ FExpr.distrib Low;;
  let high = fun () -> inj @@ FExpr.distrib High;;
  let var  = fun x  -> inj @@ FExpr.distrib (Var x);;
  
  let ifte = fun x y z -> inj @@ FStat.distrib (Ifte (x,y,z));;
  let asgn = fun x y   -> inj @@ FStat.distrib (Asgn (x,y));;
  
  let expr = fun x     -> inj @@ FGraph.distrib (Expr x);;
  let lein = fun x y z -> inj @@ FGraph.distrib (Lein (x,y,z));;
  let mux  = fun x y z -> inj @@ FGraph.distrib (Mux (x,y,z));;
  let null = fun ()    -> inj @@ FGraph.distrib Null;;
end;;


