(** linear tuples of 3, 4 and 5 entries *)
open Logic;;

(** types for linear 3-tuples *)
module Tup3 = struct
  
  @type ('a,'b,'c) t       = ('a, ('b,'c) LPair.t) LPair.t             with show, gmap;;
  
  @type ('a,'c,'e) ground  = ('a, ('c,'e) LPair.ground) LPair.ground   with show, gmap;;
  
  @type ('b,'d,'f) logic   = ('b, ('d,'f) LPair.logic) LPair.logic     with show, gmap;;
  
  type ('a,'b,'c,'d,'e,'f) groundi
  (* compatible with (('a,'c,'e) ground, ('b,'d,'f) logic) injected *)
    = ('a,'b, ('c,'e) LPair.ground, ('d,'f) LPair.logic) LPair.groundi;;

  let reify :
    (VarEnv.t -> ('a,'b) injected -> 'b) ->
    (VarEnv.t -> ('c,'d) injected -> 'd) ->
    (VarEnv.t -> ('e,'f) injected -> 'f) ->
    VarEnv.t -> ('a,'b,'c,'d,'e,'f) groundi -> ('b,'d,'f) logic
    = fun r1 r2 r3 h x -> LPair.reify r1 (LPair.reify r2 r3) h x;;
  
end;;

(** types for linear 4-tuples *)
module Tup4 = struct
  
  @type ('a,'b,'c,'d) t       = ('a, ('b,'c,'d) Tup3.t) LPair.t             with show, gmap;;
  
  @type ('a,'c,'e,'g) ground  = ('a, ('c,'e,'g) Tup3.ground) LPair.ground   with show, gmap;;
  
  @type ('b,'d,'f,'h) logic   = ('b, ('d,'f,'h) Tup3.logic) LPair.logic     with show, gmap;;
  
  type ('a,'b,'c,'d,'e,'f,'g,'h) groundi
    = ('a,'b, ('c,'e,'g) Tup3.ground, ('d,'f,'h) Tup3.logic) LPair.groundi;;
  
  let reify :
    (VarEnv.t -> ('a,'b) injected -> 'b) ->
    (VarEnv.t -> ('c,'d) injected -> 'd) ->
    (VarEnv.t -> ('e,'f) injected -> 'f) ->
    (VarEnv.t -> ('g,'h) injected -> 'h) ->
    VarEnv.t -> ('a,'b,'c,'d,'e,'f,'g,'h) groundi -> ('b,'d,'f,'h) logic
    = fun r1 r2 r3 r4 h x -> LPair.reify r1 (Tup3.reify r2 r3 r4) h x;;
end;;

(** types for linear 5-tuples *)
module Tup5 = struct
  
  @type ('a,'b,'c,'d,'e) t       = ('a, ('b,'c,'d,'e) Tup4.t) LPair.t            with show, gmap;;
  
  @type ('a,'c,'e,'g,'i) ground  = ('a, ('c,'e,'g,'i) Tup4.ground) LPair.ground  with show, gmap;;
  
  @type ('b,'d,'f,'h,'j) logic   = ('b, ('d,'f,'h,'j) Tup4.logic) LPair.logic    with show, gmap;;
  
  type ('a,'b,'c,'d,'e,'f,'g,'h,'i,'j) groundi
    = ('a,'b, ('c,'e,'g,'i) Tup4.ground, ('d,'f,'h,'j) Tup4.logic) LPair.groundi;;
  
  let reify :
    (VarEnv.t -> ('a,'b) injected -> 'b) ->
    (VarEnv.t -> ('c,'d) injected -> 'd) ->
    (VarEnv.t -> ('e,'f) injected -> 'f) ->
    (VarEnv.t -> ('g,'h) injected -> 'h) ->
    (VarEnv.t -> ('i,'j) injected -> 'j) ->
    VarEnv.t -> ('a,'b,'c,'d,'e,'f,'g,'h,'i,'j) groundi -> ('b,'d,'f,'h,'j) logic
    = fun r1 r2 r3 r4 r5 h x -> LPair.reify r1 (Tup4.reify r2 r3 r4 r5) h x;;
end;;


(* test *)
(*
let aaa : ('a,'b,'c,'d,'e,'f,'g,'h,'i,'j) Tup5.groundi
  = ocanren {(1,"hi", LOption.Some 6, LBool.falso, 'f')};;
*)
