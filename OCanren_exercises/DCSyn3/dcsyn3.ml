open OCanren;;
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

module Bool = struct
  include BooleanTypes;;
  let reify = fun h x -> reify h x;; (* the shallow reifier from [Logic] *)
  module Inj = struct let b0 = !!(O) and b1 = !!(I);; end;;
  open Inj;;
  (** test or generate *)
  let tog : groundi -> goal = fun x -> ocanren { x == b0 | x == b1 };;
end;;

module Constnt2Types = struct
  @type 'bool constnt2 = ('bool, 'bool) Pair.t       with show, gmap;;
  @type 'b t              = 'b constnt2              with show, gmap;;
  @type ground            = Bool.ground t            with show, gmap;;
  @type logic             = Bool.logic t logic'      with show, gmap;;
  type groundi           = (ground, logic) injected;;
  let fmap = fun f x -> GT.gmap(t) f x;;
end;;

module FC2 = Fmap(Constnt2Types);;

module Constnt2 = struct
  include Constnt2Types;;
  let reify : VarEnv.t -> groundi -> logic
    = fun h x -> FC2.reify Bool.reify h x;;
  let tog : groundi -> goal = fun c -> ocanren {fresh d0, d1 in c == (d1, d0) & Bool.tog d1 & Bool.tog d0};;
end;;

module Constnt3Types = struct
  @type ('b,'c2) constnt3 = ('b, 'c2) Pair.t                             with show,gmap;;
  @type ('b,'c2) t        = ('b,'c2) constnt3                            with show,gmap;;
  @type ground            = (Bool.ground, Constnt2.ground) Pair.ground   with show,gmap;;
  @type logic             = (Bool.logic, Constnt2.logic) Pair.logic      with show,gmap;;
  type groundi           = (ground, logic) injected;;
  let fmap = fun f1 f2 x -> GT.gmap(t) f1 f2 x;;
end;;

module FC3 = Fmap2(Constnt3Types);;

module Constnt3 = struct
  include Constnt3Types;;
  let reify : VarEnv.t -> groundi -> logic
    = fun h x -> FC3.reify Bool.reify Constnt2.reify h x;;
  let tog : groundi -> goal = fun c3 -> ocanren {fresh d, c2 in c3 == (d, c2) & Bool.tog d & Constnt2.tog c2};;
end;;

module Constnt4Types = struct
  @type ('b,'c3) constnt4 = ('b, 'c3) Pair.t                            with show, gmap;;
  @type ('b,'c3) t        = ('b,'c3) constnt4                           with show, gmap;;
  @type ground            = (Bool.ground, Constnt3.ground) Pair.ground  with show, gmap;;
  @type logic             = (Bool.logic, Constnt3.logic) Pair.logic     with show, gmap;;
  type groundi            = (ground, logic) injected;;
  let fmap = fun f1 f2 x -> GT.gmap(t) f1 f2 x;;
end;;

module FC4 = Fmap2(Constnt4Types);;

module Constnt4 = struct
  include Constnt4Types;;
  let reify : VarEnv.t -> groundi -> logic
    = fun h x -> FC4.reify Bool.reify Constnt3.reify h x;;
  let tog : groundi -> goal =
    fun c4 -> ocanren {fresh d, c3 in c4 == (d, c3) & Bool.tog d & Constnt3.tog c3};;
end;;

(* use four-bit constant. Change here if wider constants are used *)
module Constant = Constnt4;;

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

module FA2  = Fmap(Arr2Types);;

module Arr2 = struct
  include Arr2Types;;
  let reify  : VarEnv.t -> groundi -> logic
    = fun h x -> FA2.reify Constant.reify h x;;
  let tog : groundi -> goal =
    fun a2 -> ocanren {fresh c, c' in a2 == (c, c') & Constant.tog c & Constant.tog c'};;
end;;

module Arr4Types = struct
  @type 'arr2 arr4  = ('arr2, 'arr2) Pair.t           with show, gmap;;
  @type 'a t        = 'a arr4                         with show, gmap;;
  @type ground      = Arr2.ground t                   with show, gmap;;
  @type logic       = Arr2.logic t logic'             with show, gmap;;
   type groundi      = (ground, logic) injected;;
  let fmap = fun f x -> GT.gmap(t) f x;;
end;;

module FA4  = Fmap(Arr4Types);;

module Arr4 = struct
  include Arr4Types;;
  let reify : VarEnv.t -> groundi -> logic
    = fun h x -> FA4.reify Arr2.reify h x;;
  let tog : groundi -> goal =
    fun a4 -> ocanren {fresh a2, a2' in a4 == (a2, a2') & Arr2.tog a2 & Arr2.tog a2'};;
end;;

module Arr8Types = struct
  @type 'arr4 arr8  = ('arr4, 'arr4) Pair.t           with show, gmap;;
  @type 'a t        = 'a arr8                         with show, gmap;;
  @type ground      = Arr4.ground t                   with show, gmap;;
  @type logic       = Arr4.logic t logic'             with show, gmap;;
   type groundi      = (ground, logic) injected;;
  let fmap = fun f x -> GT.gmap(t) f x;;
end;;

module FA8  = Fmap(Arr8Types);;

module Arr8 = struct
  include Arr8Types;;
  let reify  : VarEnv.t -> groundi -> logic
    = fun h x -> FA8.reify Arr4.reify h x;;
  let tog : groundi -> goal =
    fun a8 -> ocanren {fresh a4, a4' in a8 == (a4, a4') & Arr4.tog a4 & Arr4.tog a4'};;
end;;

module Arr16Types = struct
  @type 'arr8 arr16 = ('arr8, 'arr8) Pair.t           with show, gmap;;
  @type 'a t        = 'a arr16                        with show, gmap;;
  @type ground      = Arr8.ground t                   with show, gmap;;
  @type logic       = Arr8.logic t logic'             with show, gmap;;
   type groundi      = (ground, logic) injected;;
  let fmap = fun f x -> GT.gmap(t) f x;;
end;;

module FA16 = Fmap(Arr16Types);;

module Arr16 = struct
  include Arr16Types;;
  let reify  : VarEnv.t -> groundi -> logic
    = fun h x -> FA16.reify Arr8.reify h x;;
  let tog : groundi -> goal =
    fun a16 -> ocanren {fresh a8, a8' in a16 == (a8, a8') & Arr8.tog a8 & Arr8.tog a8'};;
end;;

(* Use 16-cell arrays. Change here iff larger arrays are used  *)
module Array = Arr16;;

module ArrayAccess = struct
  (* ArrayAccess implements a binary search tree *)
  let branch :
    Bool.groundi -> ('a,'b,'a,'b) Pair.groundi -> ('a, 'b) injected -> goal
    = fun b ar c -> let open Bool.Inj in
      ocanren{ { b == b0 & fresh c' in ar == (c, c') }
             | { b == b1 & fresh c' in ar == (c', c) }};;

  let acc_arr2 :
    Bool.groundi -> Arr2.groundi -> Constant.groundi -> goal
    = fun b ar c -> branch b ar c;;

  let acc_arr4 :
    Constnt2.groundi -> Arr4.groundi -> Constant.groundi -> goal
    = fun c ar c' -> 
      ocanren{ fresh b1,b2,arr2 in
        c == (b1, b2)
        & branch b1 ar arr2
        & acc_arr2 b2 arr2 c' };;

  let acc_arr8 :
    Constnt3.groundi -> Arr8.groundi -> Constant.groundi -> goal
    = fun c ar c' -> 
      ocanren{ fresh b,c2,arr4 in
        c == (b, c2)
        & branch b ar arr4
        & acc_arr4 c2 arr4 c' };;

  let acc_arr16 :
    Constnt4.groundi -> Arr16.groundi -> Constant.groundi -> goal
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
     | Mux of 'self * 'self * 'self
     | Slice of 'self * 'self
     | Fout of 'string * 'self * 'self  (** fan out  *)
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
  let undef = fun ()    -> inj @@ FValue.distrib  (Undef)       ;;  
  let src   = fun x     -> inj @@ FSignal.distrib (Src x)       ;;
  let port  = fun x     -> inj @@ FSignal.distrib (Port x)      ;;
  let fout  = fun x y z -> inj @@ FSignal.distrib (Fout (x,y,z));;
  let mux   = fun x y z -> inj @@ FSignal.distrib (Mux (x,y,z)) ;;
  let slice = fun x y   -> inj @@ FSignal.distrib (Slice (x,y)) ;;
  include Bool.Inj;;
end;;

include Inj;;

let tup4 a b c d = Pair.pair a (Pair.pair b ( Pair.pair c d));;

(** interpreters that may produce the value [undefined]. Allowing 
      the [unndefined] value hinders synthesis. Try [test.ml].*)
module InterpA = struct
  let rec eval_imp : State.groundi -> Expr.groundi -> Value.groundi -> goal
    = fun s e v ->
  ocanren {
    {fresh c in e == Con c & v == Conv c }
  | {fresh va, r in e == Var va & List.assoco va s v}
  | {fresh va, ex, ar, idx,  ar',idx', c in
     e == Arr (va, ex)
     &
     { List.assoco va s (Arrv ar) & eval_imp s ex (Conv idx)
                                  & ArrayAccess.rel idx ar c & v == Conv c
     | List.assoco va s (Arrv ar)  & eval_imp s ex (Arrv ar')  & v == Undef
     | List.assoco va s (Conv idx) & eval_imp s ex (Arrv ar)   & v == Undef 
     | List.assoco va s (Conv idx) & eval_imp s ex (Conv idx') & v == Undef }}
  | {fresh e1,e2,e3,v' in
     e == Brh (e1,e2,e3)
     & eval_imp s e1 v'
     & {v' == Conv (tup4 b0 b0 b0 b0) & eval_imp s e3 v
      | v'=/= Conv (tup4 b0 b0 b0 b0) & eval_imp s e2 v}}};;

  let rec eval_sig : State.groundi -> Signal.groundi -> Value.groundi -> goal
    = fun s e v ->
    ocanren {
      {fresh c in e == Src c & v == Conv c }
    | {fresh va, r in e == Port va & List.assoco va s v}
    | {fresh e1,e2,e3,v' in
       e == Mux (e1,e2,e3)
       & eval_sig s e1 v'
       & { v' == Conv (tup4 b0 b0 b0 b0) & eval_sig s e3 v
         | v'=/= Conv (tup4 b0 b0 b0 b0) & eval_sig s e2 v }}
    | {fresh e1, e2, c, ar, idx, ar',idx', ar'',idx'' in
       e == Slice (e1, e2)
       & eval_sig s e1 ar
       & eval_sig s e2 idx    
       & {ar == Arrv ar' & idx == Conv idx' & ArrayAccess.rel idx' ar' c & v == Conv c
         | { ar == Arrv ar'  & idx == Arrv ar''
           | ar == Conv idx' & idx == Arrv ar'
           | ar == Conv idx' & idx == Conv idx''}
           & v == Undef}}
    | {fresh va,e1,e2,ve1,s' in
       e == Fout (va, e1, e2)
       & eval_sig s e1 ve1
       & s' == (va, ve1) :: s
       & eval_sig s' e2 v}};;
end;;

(** interpreters that do not produce the value [undefined]: taking undefined as failure  *)
module InterpB = struct
  let rec eval_imp : State.groundi -> Expr.groundi -> Value.groundi -> goal
    = fun s e v ->
      ocanren {
        {fresh c in e == Con c & v == Conv c }
      | {fresh va, r in e == Var va & List.assoco va s v}
      | {fresh va, ex, ar, idx, c in
         e == Arr (va, ex)
         & List.assoco va s (Arrv ar)
         & eval_imp s ex (Conv idx)    
         & ArrayAccess.rel idx ar c
         & v == Conv c}
      | {fresh e1,e2,e3,v' in
         e == Brh (e1,e2,e3)
         & eval_imp s e1 v'
         & {v' == Conv (tup4 b0 b0 b0 b0) & eval_imp s e3 v
          | v'=/= Conv (tup4 b0 b0 b0 b0) & eval_imp s e2 v}}};;

  let rec eval_sig : State.groundi -> Signal.groundi -> Value.groundi -> goal
    = fun s e v ->
      ocanren {
      {fresh c in e == Src c & v == Conv c }
    | {fresh va, r in e == Port va & List.assoco va s v}
    | {fresh e1,e2,e3,v' in
       e == Mux (e1,e2,e3)
       & eval_sig s e1 v'
       & { v' == Conv (tup4 b0 b0 b0 b0) & eval_sig s e3 v
         | v'=/= Conv (tup4 b0 b0 b0 b0) & eval_sig s e2 v }}
    | {fresh e1, e2, c, ar, idx, ar',idx', ar'',idx'' in
       e == Slice (e1, e2)
       & eval_sig s e1 (Arrv ar)
       & eval_sig s e2 (Conv idx)    
       & ArrayAccess.rel idx ar c
       & v == Conv c}
    | {fresh va,e1,e2,ve1,s' in
       e == Fout (va, e1, e2)
       & eval_sig s e1 ve1
       & s' == (va, ve1) :: s
       & eval_sig s' e2 v}};;

  module NoLet = struct
    let rec eval_sig : State.groundi -> Signal.groundi -> Value.groundi -> goal
      = fun s e v ->
      ocanren {
      {fresh c in e == Src c & v == Conv c }
    | {fresh va, r in e == Port va & List.assoco va s v}
    | {fresh e1,e2,e3,v' in
       e == Mux (e1,e2,e3)
       & eval_sig s e1 v'
       & { v' == Conv (tup4 b0 b0 b0 b0) & eval_sig s e3 v
         | v'=/= Conv (tup4 b0 b0 b0 b0) & eval_sig s e2 v }}
    | {fresh e1, e2, c, ar, idx, ar',idx', ar'',idx'' in
       e == Slice (e1, e2)
       & eval_sig s e1 (Arrv ar)
       & eval_sig s e2 (Conv idx)    
       & ArrayAccess.rel idx ar c
       & v == Conv c}};;
  end;;
end;;


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


let array3 :  Array.groundi =
  ocanren {((((c0,c2),(c4,c6)),((c8,c10),(c12,c14))),
            (((c1,c3),(c5,c7)),((c9,c11),(c13,c15))))};;

let state1  : State.groundi = ocanren { [("x", (Conv c1));("y",Arrv array1)] };;
let state2  : State.groundi = ocanren { [("x", (Conv c9));("y",Arrv array2)] };;
let state2b : State.groundi = ocanren { [("x", (Conv c5));("y",Arrv array2)] };;
let state2c : State.groundi = ocanren { [("x", (Conv c1));("y",Arrv array2)] };;
let state3  : State.groundi = ocanren { [("x", (Conv c3));("y",Arrv array3)] };;
let state4  : State.groundi =
  ocanren { [("x", (Conv c3));("x", (Conv c4));("x", (Conv c5));("y",Arrv array3)] };;

