open OCanren;;
open OCanren.Std;;

(** Three places to change to use wider/short constant: 
- module Constant
- module Array 
- module ArrayAccess.rel *)


(** extend the OCanren standard List (LLIst) module *)
module List  = struct
  include List;;
  
  (** An element [e] is [xinsert]ed to a list [l] only if [e] is not in the list [l] *)
  let rec xinserto : ('a,'b) injected -> ('a,'b) groundi -> ('a,'b) groundi -> goal =
    fun e l l'-> ocanren {
        l == [] & l' == [e]
      | {fresh t in l == e :: t & l' == l }
      | {fresh h,t,t' in l == h :: t & h =/= e & l' == h :: t' & xinserto e t t'}};;

  (** [xinsert] elements of [l1] to [l2] gettinng [l3] *)
  let rec xappendo = fun l1 l2 l3 -> ocanren {
    l1 == [] & l2 == l3
  | fresh h,t,l in l1 == h :: t & xinserto h l2 l & xappendo t l l3};;

end;;

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
  let grd2ijd : ground -> groundi = fun b -> !!(b);;
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
  open Bool.Inj;;
  let zero = ocanren { (b0,b0) };;
  let grd2ijd : ground -> groundi = fun (b,b') -> Pair.pair (Bool.grd2ijd b) (Bool.grd2ijd b');;
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
  open Bool.Inj;;
  let zero = ocanren { (b0,b0,b0) };;
  let grd2ijd : ground -> groundi = fun (b,c) -> Pair.pair (Bool.grd2ijd b) (Constnt2.grd2ijd c);;
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
  open Bool.Inj;;
  let zero = ocanren { (b0,b0,b0,b0) };;
  let grd2ijd : ground -> groundi = fun (b,c) -> Pair.pair (Bool.grd2ijd b) (Constnt3.grd2ijd c);;
end;;

(* use four-bit constant. Change here if wider/shorter constants are used *)
module Constant = Constnt2;;

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
  let grd2ijd : ground -> groundi = fun (c,c') -> Pair.pair (Constant.grd2ijd c) (Constant.grd2ijd c');;
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
  let grd2ijd : ground -> groundi = fun (a,a') -> Pair.pair (Arr2.grd2ijd a) (Arr2.grd2ijd a');;
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
  let grd2ijd : ground -> groundi = fun (a,a') -> Pair.pair (Arr4.grd2ijd a) (Arr4.grd2ijd a');;
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
  let grd2ijd : ground -> groundi = fun (a,a') -> Pair.pair (Arr8.grd2ijd a) (Arr8.grd2ijd a');;
end;;

(* Use 16-cell arrays. Change here iff larger/smaller arrays are used  *)
module Array = Arr4;;

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
    = fun a b c -> acc_arr4 a b c;;
  
end;;

module ValueTypes = struct
  @type ('c, 'a) value = Conv of 'c   (** constant value *)
                       | Arrv of 'a   (** array value *)
                                                                with show, gmap;;  
  @type ('a,'b) t      = ('a,'b) value                          with show, gmap;;
  @type ground         = (Constant.ground, Array.ground) t      with show, gmap;;
  @type logic          = (Constant.logic, Array.logic) t logic' with show, gmap;;
   type groundi        = (ground, logic) injected;;
  let fmap = fun f1 f2 x -> GT.gmap(t) f1 f2 x;;
end;;

module FValue = Fmap2(ValueTypes);;

module Value = struct
  include ValueTypes;;
  module Inj = struct
    let conv  = fun x -> inj @@ FValue.distrib  (Conv x);;
    let arrv  = fun x -> inj @@ FValue.distrib  (Arrv x);;
  end;;
  open Inj;;
  let grd2ijd : ground -> groundi = function
      Conv c -> conv (Constant.grd2ijd c)
    | Arrv a -> arrv (Array.grd2ijd a);;
end;;

module StateUnit = struct
  @type ground  = (GT.string, Value.ground) Pair.ground         with show, gmap;;
  @type logic   = (GT.string logic', Value.logic) Pair.logic    with show, gmap;;
  type groundi = (ground, logic) injected;;
  let grd2ijd : ground -> groundi = fun (s,v) -> Pair.pair !!(s) (Value.grd2ijd v);;
end;;

module State = struct
  @type ground  = StateUnit.ground List.ground         with show, gmap;;
  @type logic   = StateUnit.logic List.logic           with show, gmap;;
  type groundi = (ground, logic) injected;;
  let grd2ijd : ground -> groundi = fun l ->
    List.list @@ Stdlib.List.map StateUnit.grd2ijd @@ List.to_list id l;;
end;;

module Spec = struct
  @type ground = (State.ground, Value.ground) Pair.ground with show, gmap;;
  @type logic = (State.logic, Value.logic) Pair.logic with show, gmap;;
  type groundi = (ground, logic) injected;;
  let grd2ijd : ground -> groundi = fun (s,v) -> Pair.pair (State.grd2ijd s) (Value.grd2ijd v);;
end;;

module Specs = struct
  @type ground = Spec.ground List.ground with show,gmap;;
  @type logic = Spec.logic List.logic with show,gmap;;
  type groundi = (ground, logic) injected;;
  let grd2ijd : Spec.ground GT.list -> groundi =
    fun l -> List.list @@ Stdlib.List.map Spec.grd2ijd l;;
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
  module Inj = struct
    let con   = fun x     -> inj @@ FExpr.distrib   (Con x)       ;;
    let var   = fun x     -> inj @@ FExpr.distrib   (Var x)       ;;
    let arr   = fun x y   -> inj @@ FExpr.distrib   (Arr (x,y))   ;;
    let brh   = fun x y z -> inj @@ FExpr.distrib   (Brh (x,y,z)) ;;
  end;;
  open Bool.Inj;; open Inj;;
  (** find the set of all free variables *)
  let rec free_var : groundi -> 'fvar -> goal = fun p vs -> ocanren {
        {fresh c in p == Con c & vs == []}
      | {fresh v in p == Var v & vs == [(b0, v)]}      (** give informal types for vars *)
      | {fresh v,e,vs' in p == Arr (v,e) & free_var e vs' & List.xinserto (b1,v) vs' vs}
      | {fresh e1,e2,e3,v1,v2,v3,vs' in
            p == Brh (e1,e2,e3)
            & free_var e1 v1
            & free_var e2 v2
            & free_var e3 v3
            & List.xappendo v1 v2 vs'
            & List.xappendo vs' v3 vs}};;
  (* In an imperative program the same string should not appear for the Var and Arr, but this is
  not enforced and the consequence is that they will be distinguished by free_var *)
  open Value.Inj;;
  (** find all possible states for a given set of free variables *)
  let rec var_state : 'fvar -> State.groundi -> goal = fun vs sts -> ocanren {
    vs == [] & sts == []
  | fresh n,v,sts',vs',c,a in
       sts == (n,v) :: sts'
       & { vs == (b0, n) :: vs' & v == Conv c & Constant.tog c
         | vs == (b1, n) :: vs' & v == Arrv a & Array.tog a  }
       & var_state vs' sts'};;

(** relate a program [e] with the number [n] of its constructors *)
   let rec size : groundi -> Nat.groundi-> goal
    = fun e n ->
      ocanren {
        {fresh c in e == Con c & n == Nat.one}
      | {fresh va in e == Var va & n == Nat.one}
      | {fresh va, ex, n' in
         e == Arr (va, ex)
         & n == Nat.S n'
         & size ex n'}
      | {fresh e1,e2,e3,n1,n2,n3,n',n'' in
         e == Brh (e1,e2,e3)
         & n == Nat.S n''
         & size e1 n1
         & size e2 n2
         & size e3 n3
         & Nat.addo n1 n2 n'
         & Nat.addo n3 n' n''}};;
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
  module Inj = struct
    let src   = fun x     -> inj @@ FSignal.distrib (Src x)       ;;
    let port  = fun x     -> inj @@ FSignal.distrib (Port x)      ;;
    let fout  = fun x y z -> inj @@ FSignal.distrib (Fout (x,y,z));;
    let mux   = fun x y z -> inj @@ FSignal.distrib (Mux (x,y,z)) ;;
    let slice = fun x y   -> inj @@ FSignal.distrib (Slice (x,y)) ;;
  end;;
  open Bool.Inj;; open Inj;;
  (** find the set of all free variables *)
  let rec free_var : groundi -> 'fvar -> goal = fun p vs -> ocanren {
        {fresh c in p == Src c & vs == []}
      | {fresh v in p == Port v & vs == [v]} 
      | {fresh e1,e2,vs1,vs2 in
         p == Slice (e1,e2)
         & free_var e1 vs1
         & free_var e2 vs2
         & List.xappendo  vs1 vs2 vs}
      | {fresh e1,e2,e3,v1,v2,v3,vs' in
            p == Mux (e1,e2,e3)
            & free_var e1 v1
            & free_var e2 v2
            & free_var e3 v3
            & List.xappendo v1 v2 vs'
            & List.xappendo vs' v3 vs}};;

  open Value.Inj;;
  (** find all possible states for a given set of free variables *)
  let rec var_state : 'fvar -> State.groundi -> goal = fun vs sts -> ocanren {
    vs == [] & sts == []
  | fresh n,v,sts',vs',c,a in
    sts == (n,v) :: sts'
    & vs == n :: vs'
    & {v == Conv c & Constant.tog c | v == Arrv a & Array.tog a}
    & var_state vs' sts'};; 
end;;

module Inj = struct
  include Bool.Inj;;
  include Value.Inj;;
  include Expr.Inj;;
  include Signal.Inj;;
end;;

include Inj;;

module Interp = struct
  let rec eval_imp : State.groundi -> Expr.groundi -> Value.groundi -> goal
    = fun s e v ->
      ocanren {
        {fresh c in e == Con c & v == Conv c }
      | {fresh va in e == Var va & List.assoco va s v}
      | {fresh va, ex, ar, idx, c in
         e == Arr (va, ex)
         & List.assoco va s (Arrv ar)
         & eval_imp s ex (Conv idx)    
         & ArrayAccess.rel idx ar c
         & v == Conv c}
      | {fresh e1,e2,e3,v' in
         e == Brh (e1,e2,e3)
         & eval_imp s e1 v'
         & {v' == Conv Constant.zero & eval_imp s e3 v
          | v'=/= Conv Constant.zero & eval_imp s e2 v}}};;

  let rec eval_sig : State.groundi -> Signal.groundi -> Value.groundi -> goal
    = fun s e v ->
      ocanren {
      {fresh c in e == Src c & v == Conv c }
    | {fresh va in e == Port va & List.assoco va s v}
    | {fresh e1,e2,e3,v' in
       e == Mux (e1,e2,e3)
       & eval_sig s e1 v'
       & { v' == Conv Constant.zero & eval_sig s e3 v
         | v'=/= Conv Constant.zero & eval_sig s e2 v }}
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
    | {fresh va in e == Port va & List.assoco va s v}
    | {fresh e1,e2,e3,v' in
       e == Mux (e1,e2,e3)
       & eval_sig s e1 v'
       & { v' == Conv Constant.zero & eval_sig s e3 v
         | v'=/= Conv Constant.zero & eval_sig s e2 v }}
    | {fresh e1, e2, c, ar, idx, ar',idx', ar'',idx'' in
       e == Slice (e1, e2)
       & eval_sig s e1 (Arrv ar)
       & eval_sig s e2 (Conv idx)    
       & ArrayAccess.rel idx ar c
       & v == Conv c}};;

    let rec syn : Specs.groundi -> Signal.groundi -> goal = fun ss p -> ocanren {
        ss == []
      | fresh st,va,ss' in ss == (st, va) :: ss' & eval_sig st p va & syn ss' p };; 
  end;;
end;;

(* interpreters with size bound *)
module InterpSZ = struct
  (** Given state [s], the program [e] evaluates to [v], where [e] has [n] constructors *)
   let rec eval_imp : State.groundi -> Expr.groundi -> Value.groundi -> Nat.groundi-> goal
    = fun s e v n ->
      ocanren {
        {fresh c in e == Con c & v == Conv c & n == Nat.one}
      | {fresh va in e == Var va & List.assoco va s v & n == Nat.one}
      | {fresh va, ex, ar, idx, c, n' in
         e == Arr (va, ex)
         & v == Conv c
         & n == Nat.S n'
         & List.assoco va s (Arrv ar)
         & eval_imp s ex (Conv idx) n'   
         & ArrayAccess.rel idx ar c
          }
      | {fresh e1,e2,e3,v1,v2,v3,n1,n2,n3,n',n'' in
         e == Brh (e1,e2,e3)
         & {v1 == Conv Constant.zero & v == v3
           | v1 =/= Conv Constant.zero & v == v2}
         & n == Nat.S n''
         & eval_imp s e1 v1 n1
         & eval_imp s e2 v2 n2
         & eval_imp s e3 v3 n3
         & Nat.addo n1 n2 n'
         & Nat.addo n3 n' n''}};;

  module NoLet = struct
    let rec eval_sig : State.groundi -> Signal.groundi -> Value.groundi -> Nat.groundi -> goal
      = fun s e v n ->
      ocanren {
      {fresh c in e == Src c & v == Conv c & n == Nat.one}
    | {fresh va in e == Port va & List.assoco va s v & n == Nat.one}
    | {fresh e1,e2,e3,v1,v2,v3,n1,n2,n3,n',n'' in
       e == Mux (e1,e2,e3)
       & { v1 == Conv Constant.zero & v == v3
         | v1 =/= Conv Constant.zero & v == v2 }
       & n == Nat.S n''
       & eval_sig s e1 v1 n1
       & eval_sig s e2 v2 n2
       & eval_sig s e3 v3 n3
       & Nat.addo n3 n' n''
       & Nat.addo n1 n2 n'}
    | {fresh e1, e2, c, ar, idx, ar',idx', ar'',idx'',n1,n2,n' in
       e == Slice (e1, e2)
       & v == Conv c
       & n == Nat.S n'
       & eval_sig s e1 (Arrv ar) n1
       & eval_sig s e2 (Conv idx) n2
       & Nat.addo n1 n2 n'
       & ArrayAccess.rel idx ar c}};;

    let rec syn : Specs.groundi -> Signal.groundi -> Nat.groundi -> goal =
      fun ss p n -> ocanren {
        ss == []
      | fresh st,va,ss' in ss == (st, va) :: ss' & eval_sig st p va n & syn ss' p n};; 
  end;;
end;;


module TwoBit = struct
  let c0  : Constnt2.groundi = ocanren { (b0,b0) };;
  let c1  : Constnt2.groundi = ocanren { (b0,b1) };;
  let c2  : Constnt2.groundi = ocanren { (b1,b0) };;
  let c3  : Constnt2.groundi = ocanren { (b1,b1) };;
end;;



