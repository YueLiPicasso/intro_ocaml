(** THe type of a particular trie *)
open Logic;;
open ExpoTuples;;

@type 'a logic' = 'a logic with show, gmap;; 

module T = struct
  @type ('a,'b) t = {table : 'a ; alpha : 'b} with show, gmap;;
  let fmap = fun f1 f2 x -> GT.gmap(t) f1 f2 x;;
end;;

include T;;

@type ground = (Register.ground Expo2.ground,  Cell.ground) t with show, gmap;;

@type logic = (Register.logic Expo2.logic, Cell.logic) t logic' with show, gmap;;

type groundi = (ground, logic) injected;;

module FT :
sig
  val distrib :
    (('a, 'c) Logic.injected, ('b, 'd) Logic.injected) T.t ->
    (('a, 'b) T.t, ('c, 'd) T.t) Logic.injected
  val reify :
    (VarEnv.t -> ('a, 'b) Logic.injected -> 'b) ->
    (VarEnv.t -> ('c, 'd) Logic.injected -> 'd) ->
    VarEnv.t ->
    (('a, 'c) T.t, ('b, 'd) T.t Logic.logic) Logic.injected ->
    ('b, 'd) T.t Logic.logic
  val prjc :
    (VarEnv.t -> ('a, 'b) Logic.injected -> 'a) ->
    (VarEnv.t -> ('c, 'd) Logic.injected -> 'c) ->
    (int -> ('a, 'c) T.t list -> ('a, 'c) T.t) ->
    VarEnv.t ->
    (('a, 'c) T.t, ('b, 'd) T.t Logic.logic) Logic.injected -> ('a, 'c) T.t
end
  = Fmap2(T);;

let reify : VarEnv.t -> groundi -> logic = fun h x ->
  FT.reify (Expo2.reify Register.reify) (Cell.reify) h x;;

let trie : ((Register.ground, Register.logic) Expo2.groundi, Cell.groundi) T.t -> groundi
  = fun x -> inj @@ FT.distrib x;;



