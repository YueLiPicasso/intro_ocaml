(** THe type of a particular trie *)
open Logic;;
open LinearTuples;;
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

module FT = Fmap2(T);;

let reify : VarEnv.t -> groundi -> logic = fun h x ->
  FT.reify (Expo2.reify Register.reify) (Cell.reify) h x;;

let trie : ((Register.ground, Register.logic) Expo2.groundi, Cell.groundi) T.t -> groundi
  = fun x -> inj @@ FT.distrib x;;

(** the initial trie state *)
let inito : groundi =
  let open LOption in let open LBool in
    trie {table = Expo2.expo2 Register.inito ;
          alpha = some (Tup4.tuple falso falso falso truo)};;

let _ = print_string @@ (GT.show(ground) @@ prj inito) ^ "\n";;


