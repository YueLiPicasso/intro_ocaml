open OCanren;;
module L = List ;;
open OCanren.Std;;

(* aliasing the standard OCanren type constructor [logic] *)
@type 'a logic' = 'a logic with show, gmap;;

(* constants in different sizes: constntN is an N-bit binary number. 
   constnt1 is just boolean. *)

module BooleanTypes = struct
  @type boolean = O | I                        with show, gmap;;
  @type t       = boolean                      with show, gmap;;
  @type ground  = t                            with show, gmap;;
  @type logic   = t logic'                     with show, gmap;;
   type groundi = (ground, logic) injected;;
end;;

module Constnt2Types = struct
  @type 'boolean constnt2 = ('boolean, 'boolean) Pair.t      with show, gmap;;
  @type 'b t              = 'b constnt2                      with show, gmap;;
  @type ground            = BooleanTypes.ground t            with show, gmap;;
  @type logic             = BooleanTypes.logic t logic'      with show, gmap;;
   type groundi           = (ground, logic) injected;;
  let fmap = fun f x -> GT.gmap(t) f x;;
end;;

module Constnt3Types = struct
  @type ('b,'c2) constnt3 = ('b, 'c2) Pair.t                                          with show, gmap;;
  @type ('b,'c2) t        = ('b,'c2) constnt3                                         with show,gmap;;
  @type ground            = (BooleanTypes.ground, Constnt2Types.ground) Pair.ground   with show,gmap;;
  @type logic             = (BooleanTypes.logic, Constnt2Types.logic) Pair.logic      with show,gmap;;
   type groundi           = (ground, logic) injected;;
  let fmap = fun f1 f2 x -> GT.gmap(t) f1 f2 x;;
end;;

module Constnt4Types = struct
  @type ('b,'c3) constnt4 = ('b, 'c3) Pair.t                                         with show, gmap;;
  @type ('b,'c3) t        = ('b,'c3) constnt4                                        with show, gmap;;
  @type ground            = (BooleanTypes.ground, Constnt3Types.ground) Pair.ground  with show, gmap;;
  @type logic             = (BooleanTypes.logic, Constnt3Types.logic) Pair.logic     with show, gmap;;
   type groundi            = (ground, logic) injected;;
  let fmap = fun f1 f2 x -> GT.gmap(t) f1 f2 x;;
end;;

module FC2 = Fmap(Constnt2Types);;
module FC3 = Fmap2(Constnt3Types);;
module FC4 = Fmap2(Constnt4Types);;

let c2reify : VarEnv.t -> Constnt2Types.groundi -> Constnt2Types.logic = fun h x -> FC2.reify reify h x;;
let c3reify : VarEnv.t -> Constnt3Types.groundi -> Constnt3Types.logic = fun h x -> FC3.reify reify c2reify h x;;
let c4reify : VarEnv.t -> Constnt4Types.groundi -> Constnt4Types.logic = fun h x -> FC4.reify reify c3reify h x;;

(* use four-bit constant. Change here if wider constants are used *)
module Constant = struct
  include Constnt4Types;;
  let reify = fun h x -> c4reify h x;;
end;;

(* arrays in different sizes: arrN is an N-cell array, 
   and each cell holds a constant *)

module Arr2Types = struct
  @type 'constnt arr2  = ('constnt, 'constnt) Pair.t       with show, gmap;;
  @type 'c t           = 'c arr2                           with show, gmap;;
  @type ground         = Constant.ground t                 with show, gmap;;
  @type logic          = Constant.logic t logic'           with show, gmap;;
   type groundi         = (ground, logic) injected;;
  let fmap = fun f x -> GT.gmap(t) f x;;
end;;

module Arr4Types = struct
  @type 'arr2 arr4  = ('arr2, 'arr2) Pair.t                with show, gmap;;
  @type 'a t        = 'a arr4                              with show, gmap;;
  @type ground      = Arr2Types.ground t                   with show, gmap;;
  @type logic       = Arr2Types.logic t logic'             with show, gmap;;
   type groundi      = (ground, logic) injected;;
  let fmap = fun f x -> GT.gmap(t) f x;;
end;;

module Arr8Types = struct
  @type 'arr4 arr8  = ('arr4, 'arr4) Pair.t                with show, gmap;;
  @type 'a t        = 'a arr8                              with show, gmap;;
  @type ground      = Arr4Types.ground t                   with show, gmap;;
  @type logic       = Arr4Types.logic t logic'             with show, gmap;;
   type groundi      = (ground, logic) injected;;
  let fmap = fun f x -> GT.gmap(t) f x;;
end;;

module Arr16Types = struct
  @type 'arr8 arr16 = ('arr8, 'arr8) Pair.t                with show, gmap;;
  @type 'a t        = 'a arr16                             with show, gmap;;
  @type ground      = Arr8Types.ground t                   with show, gmap;;
  @type logic       = Arr8Types.logic t logic'             with show, gmap;;
   type groundi      = (ground, logic) injected;;
  let fmap = fun f x -> GT.gmap(t) f x;;
end;;

module FA2  = Fmap(Arr2Types);;
module FA4  = Fmap(Arr4Types);;
module FA8  = Fmap(Arr8Types);;
module FA16 = Fmap(Arr16Types);;

let a2reify  : VarEnv.t -> Arr2Types.groundi -> Arr2Types.logic   = fun h x -> FA2.reify Constant.reify h x;;
let a4reify  : VarEnv.t -> Arr4Types.groundi -> Arr4Types.logic   = fun h x -> FA4.reify a2reify h x;;
let a8reify  : VarEnv.t -> Arr8Types.groundi -> Arr8Types.logic   = fun h x -> FA8.reify a4reify h x;;
let a16reify : VarEnv.t -> Arr16Types.groundi -> Arr16Types.logic = fun h x -> FA16.reify a8reify h x;;

(* Use 16-cell arrays. Change here iff larger arrays are used  *)
module Array = struct
  include Arr16Types;;
  let reify = fun h x -> a16reify h x;;
end;; 

module ArrayAccess = struct
  (* ArrayAccess implements a binary search tree *)
  let branch :
    BooleanTypes.groundi ->
    ('a,'b,'a,'b) Pair.groundi -> ('a, 'b) injected -> goal
    = fun b ar c -> let b0 = !!(BooleanTypes.O) and b1 = !!(BooleanTypes.I) in
      ocanren{ { b == b0 & fresh c' in ar == (c, c') }
             | { b == b1 & fresh c' in ar == (c', c) }};;

  let acc_arr2 :
    BooleanTypes.groundi -> Arr2Types.groundi -> Constant.groundi -> goal
    = fun b ar c -> branch b ar c;;

  let acc_arr4 :
    Constnt2Types.groundi -> Arr4Types.groundi -> Constant.groundi -> goal
    = fun c ar c' -> 
      ocanren{ fresh b1,b2,arr2 in
        c == (b1, b2)
        & branch b1 ar arr2
        & acc_arr2 b2 arr2 c' };;

  let acc_arr8 :
    Constnt3Types.groundi -> Arr8Types.groundi -> Constant.groundi -> goal
    = fun c ar c' -> 
      ocanren{ fresh b,c2,arr4 in
        c == (b, c2)
        & branch b ar arr4
        & acc_arr4 c2 arr4 c' };;

  let acc_arr16 :
    Constnt4Types.groundi -> Arr16Types.groundi -> Constant.groundi -> goal
    = fun c ar c' -> 
      ocanren{ fresh b,c3,arr8 in
        c == (b, c3)
        & branch b ar arr8
        & acc_arr8 c3 arr8 c' };;

  (* The default access method *)
  let rel :
    Constant.groundi -> Array.groundi -> Constant.groundi -> goal
    = fun a b c -> acc_arr16 a b c;;
  
end;;

module Value = struct
  @type ('c, 'a) value = Conv of 'c   (** constant value *)
                       | Arrv of 'a   (** array value *)
                       | Undef        (** undefined *)
                                                                with show, gmap;;  
  @type ('a,'b) t      = ('a,'b) value                          with show, gmap;;
  @type ground         = (Constant.ground, Array.ground) t      with show, gmap;;
  @type logic          = (Constant.logic, Array.logic) t logic' with show, gmap;;
   type groundi        = (ground, logic) injected;;
  let fmap = fun f1 f2 x -> GT.gmap(t) f1 f2 x;;
end;;

module FValue = Fmap2(Value);;

module StateUnit = struct
  @type ground  = (GT.string, Value.ground) Pair.ground         with show, gmap;;
  @type logic   = (GT.string logic', Value.logic) Pair.logic    with show, gmap;;
   type groundi = (ground, logic) injected;;
end;;

module State = struct
  @type ground  = StateUnit.ground List.ground         with show, gmap;;
  @type logic   = StateUnit.logic List.logic           with show, gmap;;
   type groundi = (ground, logic) injected;;
end;;

module ExprTypes = struct
  @type ('c,'v,'self) expr = Con of 'c
                           | Var of 'v  (** a variable is a character string *)
                           | Arr of 'v * 'self
                           | Brh of 'self * 'self * 'self
                                                                                   with show, gmap;;
  @type ('a,'b,'c) t       = ('a,'b,'c) expr                                       with show, gmap;;
  @type ground             = (Constant.ground, GT.string, ground) t                with show, gmap;;
  @type logic              = (Constant.logic, GT.string logic', logic) t logic'    with show, gmap;;
   type groundi = (ground, logic) injected;;
  let fmap = fun f1 f2 f3 x -> GT.gmap(t) f1 f2 f3 x;;
end;;

module FExpr = Fmap3(ExprTypes);;

module Expr = struct
  include ExprTypes;;
  let rec reify = fun h x -> FExpr.reify Constant.reify Logic.reify reify h x;;
end;;

module SignalTypes = struct
  @type ('cons, 'string, 'self) signal =
       Src of 'cons                     (** constant *)
     | Port of 'string                  (** variable *)
     | Fout of 'string * 'self * 'self  (** fan out  *)
     | Mux of 'self * 'self * 'self
     | Slice of 'self * 'self
   with show, gmap;;
  @type ('a,'b,'c) t = ('a,'b,'c) signal with show, gmap;;
  @type ground = (Constant.ground, GT.string, ground) t with show, gmap;;
  @type logic = (Constant.logic, GT.string logic', logic) t logic'
   with show, gmap;;
  type groundi = (ground, logic) injected;;
  let fmap = fun f1 f2 f3 x -> GT.gmap(t) f1 f2 f3 x;;
end;;

module FSignal = Fmap3(SignalTypes);;

module Signal = struct
  include SignalTypes;;
  let rec reify = fun h x ->
    FSignal.reify Constant.reify Logic.reify reify h x;;
end;;

module Inj = struct
  let con   = fun x     -> inj @@ FExpr.distrib   (Con x)       ;;
  let var   = fun x     -> inj @@ FExpr.distrib   (Var x)       ;;
  let arr   = fun x y   -> inj @@ FExpr.distrib   (Arr (x,y))   ;;
  let brh   = fun x y z -> inj @@ FExpr.distrib   (Brh (x,y,z)) ;;
  let conv  = fun x     -> inj @@ FValue.distrib  (Conv x)      ;;
  let arrv  = fun x     -> inj @@ FValue.distrib  (Arrv x)      ;;
  let undef = fun ()    -> inj @@ FValue.distrib  (Undef)       ;;  (* not used so far *)
  let src   = fun x     -> inj @@ FSignal.distrib (Src x)       ;;
  let port  = fun x     -> inj @@ FSignal.distrib (Port x)      ;;
  let fout  = fun x y z -> inj @@ FSignal.distrib (Fout (x,y,z));;
  let mux   = fun x y z -> inj @@ FSignal.distrib (Mux (x,y,z)) ;;
  let slice = fun x y   -> inj @@ FSignal.distrib (Slice (x,y)) ;;
  let b0 = !!(BooleanTypes.O)
  and b1 = !!(BooleanTypes.I);;
end;;

let rec eval_imp : State.groundi -> Expr.groundi -> Value.groundi -> goal
  = fun s e v ->
    let open Inj in
    let tup4 a b c d = Pair.pair a (Pair.pair b ( Pair.pair c d)) in 
  ocanren {
    {fresh c in e == Con c & v == Conv c }
  | {fresh va, r in e == Var va & List.assoco va s v}
  | {fresh va, ex, ar, idx, c in
     e == Arr (va, ex)
     & List.assoco va s (Arrv ar)
     & eval_imp s ex (Conv idx)   (** invalid array access (var not an array or index not a constant) fails the goal *)
     & ArrayAccess.rel idx ar c
     & v == Conv c }
  | {fresh e1,e2,e3,v' in
     e == Brh (e1,e2,e3)
     & eval_imp s e1 v'
     & {v' == Conv (tup4 b0 b0 b0 b0) & eval_imp s e3 v | v'=/= Conv (tup4 b0 b0 b0 b0) & eval_imp s e2 v}}
  };;

let rec eval_sig : State.groundi -> Signal.groundi -> Value.groundi -> goal
  = fun s e v ->
    let open Inj in
    let tup4 a b c d = Pair.pair a (Pair.pair b ( Pair.pair c d)) in 
    ocanren {
      {fresh c in e == Src c & v == Conv c }
    | {fresh va, r in e == Port va & List.assoco va s v}
    | {fresh e1,e2,e3,v' in
       e == Mux (e1,e2,e3)
       & eval_sig s e1 v'
       & { v' == Conv (tup4 b0 b0 b0 b0) & eval_sig s e3 v
         | v'=/= Conv (tup4 b0 b0 b0 b0) & eval_sig s e2 v }}
    | {fresh e1, e2, ar, idx, c in
       e == Slice (e1, e2)
       & eval_sig s e1 (Arrv ar)
       & eval_sig s e2 (Conv idx)   (** invalid array access (var not an array or index not a constant) fails the goal *)
       & ArrayAccess.rel idx ar c
       & v == Conv c }
    };;

(* last case : fan out *)

open Inj;;

let c0  : Constant.groundi = ocanren { (b0,b0,b0,b0) };;
let c1  : Constant.groundi = ocanren { (b0,b0,b0,b1) };;
let c2  : Constant.groundi = ocanren { (b0,b0,b1,b0) };;
let c3  : Constant.groundi = ocanren { (b0,b0,b1,b1) };;
let c4  : Constant.groundi = ocanren { (b0,b1,b0,b0) };;
let c5  : Constant.groundi = ocanren { (b0,b1,b0,b1) };;
let c6  : Constant.groundi = ocanren { (b0,b1,b1,b0) };;
let c7  : Constant.groundi = ocanren { (b0,b1,b1,b1) };;
let c8  : Constant.groundi = ocanren { (b1,b0,b0,b0) };;
let c9  : Constant.groundi = ocanren { (b1,b0,b0,b1) };;
let c10 : Constant.groundi = ocanren { (b1,b0,b1,b0) };;
let c11 : Constant.groundi = ocanren { (b1,b0,b1,b1) };;
let c12 : Constant.groundi = ocanren { (b1,b1,b0,b0) };;
let c13 : Constant.groundi = ocanren { (b1,b1,b0,b1) };;
let c14 : Constant.groundi = ocanren { (b1,b1,b1,b0) };;
let c15 : Constant.groundi = ocanren { (b1,b1,b1,b1) };;



let array1 : Array.groundi = ocanren {
    (((((b0,b0,b0,b0),
        (b0,b0,b0,b1)),
       ((b0,b0,b1,b0),
        (b0,b0,b1,b1))),
      (((b0,b1,b0,b0),
        (b0,b1,b0,b1)),
       ((b0,b1,b1,b0),
        (b0,b1,b1,b1)))),
     ((((b1,b0,b0,b0),
        (b1,b0,b0,b1)),
       ((b1,b0,b1,b0),
        (b1,b0,b1,b1))),
      (((b1,b1,b0,b0),
        (b1,b1,b0,b1)),
       ((b1,b1,b1,b0),
        (b1,b1,b1,b1)))))
  };;


let array2 : Array.groundi = ocanren {
    (((((b1,b0,b0,b0),
        (b0,b1,b0,b0)),
       ((b0,b0,b1,b0),
        (b0,b0,b0,b1))),
      (((b1,b1,b0,b0),
        (b0,b1,b1,b0)),
       ((b0,b0,b1,b1),
        (b1,b0,b0,b1)))),
     ((((b1,b1,b1,b0),
        (b0,b1,b1,b1)),
       ((b1,b0,b1,b1),
        (b1,b1,b0,b1))),
      (((b1,b0,b1,b0),
        (b0,b1,b0,b1)),
       ((b1,b1,b1,b1),
        (b0,b0,b0,b0)))))
  };;


let array3 :  Array.groundi = ocanren {((((c0,c2),(c4,c6)),((c8,c10),(c12,c14))),(((c1,c3),(c5,c7)),((c9,c11),(c13,c15))))};;

let state1  : State.groundi = ocanren { [("x", (Conv c1));("y",Arrv array1)] };;
let state2  : State.groundi = ocanren { [("x", (Conv c9));("y",Arrv array2)] };;
let state2b : State.groundi = ocanren { [("x", (Conv c5));("y",Arrv array2)] };;
let state2c : State.groundi = ocanren { [("x", (Conv c1));("y",Arrv array2)] };;
let state3  : State.groundi = ocanren { [("x", (Conv c3));("y",Arrv array3)] };;

(* test array access *)

let _ = 
  L.iter (fun x -> print_string @@ GT.show(Constant.ground) x;print_newline())
  @@ Stream.take ~n:3 @@ 
  run q (fun q -> ocanren {ArrayAccess.rel c1 array1 q}) project;;


let _ = 
  L.iter (fun x -> print_string @@ GT.show(Array.logic) x;print_newline())
    @@  Stream.take ~n:3 @@ 
  run q (fun q -> ocanren {ArrayAccess.rel c1 q c1}) (fun q -> q#reify(Array.reify));;

(* test eval_imp *)

(* eval constant *)

let _ =
  L.iter (fun x -> print_string @@ GT.show(Value.ground) x;print_newline())
  @@ Stream.take ~n:3 @@
  run q (fun q -> ocanren {fresh r in eval_imp r (Con c1) q}) project;;

(* eval variable *)

let _ =
  L.iter (fun x -> print_string @@ GT.show(Value.ground) x;print_newline())
  @@ Stream.take ~n:3 @@
  run q (fun q -> ocanren {eval_imp state1 (Var "x") q}) project;;


let _ =
  L.iter (fun x -> print_string @@ GT.show(Value.ground) x;print_newline())
  @@ Stream.take ~n:3 @@
  run q (fun q -> ocanren {eval_imp state1 (Var "y") q}) project;;


(* eval array *)

let _ =
  L.iter (fun x -> print_string @@ GT.show(Value.ground) x;print_newline())
  @@ Stream.take ~n:3 @@
  run q (fun q -> ocanren {eval_imp state1 (Arr ("y", Con c1)) q}) project;;


let _ =
  L.iter (fun x -> print_string @@ GT.show(Value.ground) x;print_newline())
  @@ Stream.take ~n:3 @@
  run q (fun q -> ocanren {eval_imp state1 (Arr ("y", Var "x")) q}) project;;


let _ =
  L.iter (fun x -> print_string @@ GT.show(Value.ground) x;print_newline())
  @@ Stream.take ~n:3 @@
  run q (fun q -> ocanren {eval_imp state1 (Arr ("y", Arr ("y", Var "x"))) q}) project;;


let _ =
  L.iter (fun x -> print_string @@ GT.show(Value.ground) x;print_newline())
  @@ Stream.take ~n:3 @@
  run q (fun q -> ocanren {eval_imp state1 (Arr ("y", Arr ("y", Arr ("y", Var "x")))) q}) project;;
(* potential optimization: store the value of "y" to avoid repeated lookup *)

(* invalid array access gives an empty set of answers *)

let _ =
  L.iter (fun x -> print_string @@ GT.show(Value.ground) x;print_newline())
  @@ Stream.take ~n:3 @@
  run q (fun q -> ocanren {eval_imp state1 (Arr ("x", Con c1)) q}) project;;

let _ =
  L.iter (fun x -> print_string @@ GT.show(Value.ground) x;print_newline())
  @@ Stream.take ~n:3 @@
  run q (fun q -> ocanren {eval_imp state1 (Arr ("y", Var "y")) q}) project;;


(* eval branch *)

let _ =
  L.iter (fun x -> print_string @@ GT.show(Value.ground) x;print_newline())
  @@ Stream.take ~n:3 @@
  run q (fun q -> ocanren {eval_imp state1 (Brh(Con c0, Con c0, Con c1)) q}) project;;

let _ =
  L.iter (fun x -> print_string @@ GT.show(Value.ground) x;print_newline())
  @@ Stream.take ~n:3 @@
  run q (fun q -> ocanren {eval_imp state1 (Brh(Con c1, Con c0, Con c1)) q}) project;;

let _ =
  L.iter (fun x -> print_string @@ GT.show(Value.ground) x;print_newline())
  @@ Stream.take ~n:3 @@
  run q (fun q -> ocanren {eval_imp state1 (Brh(Brh(Arr("y", Con c0),Con c1,Con c9), Con c0, Con c1)) q}) project;;

let _ =
  L.iter (fun x -> print_string @@ GT.show(Value.ground) x;print_newline())
  @@ Stream.take ~n:3 @@
  run q (fun q -> ocanren {eval_imp state1 (Brh(Con c1,Brh(Arr("y", Con c0), Con c9, Con c1), Con c0)) q}) project;;

(* given a state and a result, synthesis programs *)

let _ =
  L.iter (fun x -> print_string @@ GT.show(Expr.logic) x;print_newline())
  @@ Stream.take ~n:5 @@ (* as many as you want *)
  run q (fun q -> ocanren {eval_imp state1 q (Conv c1)}) (fun q -> q#reify(Expr.reify));;

(* given two  state-result pairs, synthesis programs *)

let _ =  print_newline();;

let _ =
  L.iter (fun x -> print_string @@ GT.show(Expr.logic) x;print_newline())
  @@ Stream.take ~n:5 @@ (* as many as you want *)
  run q (fun q -> ocanren {eval_imp state1 q (Conv c1) & eval_imp state2 q (Conv c2)}) (fun q -> q#reify(Expr.reify));;


(*
(* given three  state-result pairs, synthesis programs : hard challenge, processs killed *)

let _ =  print_newline();;

let _ =
  L.iter (fun x -> print_string @@ GT.show(Expr.logic) x;print_newline())
  @@ Stream.take ~n:1 @@
  run q (fun q -> ocanren {eval_imp state2 q (Conv c1) & eval_imp state2b q (Conv c2) & eval_imp state2c q (Conv c3)}) (fun q -> q#reify(Expr.reify));;

(* given three  state-result pairs, synthesis programs : hard challenge, processs killed *)

let _ =  print_newline();;

let _ =
  L.iter (fun x -> print_string @@ GT.show(Expr.logic) x;print_newline())
  @@ Stream.take ~n:1 @@
  run q (fun q -> ocanren {eval_imp state1 q (Conv c5) & eval_imp state2 q (Conv c10) & eval_imp state3 q (Conv c15)}) (fun q -> q#reify(Expr.reify));;
*)


(* test filtero *)

let sunit_test : StateUnit.groundi -> Bool.groundi -> goal =
  fun st bl ->
  ocanren {
    {fresh v in st == ("x", v) & bl == Bool.falso}
  | {fresh k,v in st == (k, v) & k =/= "x" & bl == Bool.truo}
  };;

let _ =  print_newline();;

let _ =
  L.iter (fun x -> print_string @@ GT.show(State.ground) x;print_newline())
  @@  Stream.take ~n:3 @@
  run q (fun q -> List.filtero sunit_test state1 q) project;;
