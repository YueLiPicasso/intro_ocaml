(** Syntax and interpreters of the two langauges *)
open OCanren;;
open OCanren.Std;;
open Coar;;


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



