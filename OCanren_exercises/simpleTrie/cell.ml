(** The type of a cell in a register *)
open LinearTuples;;

@type t =
   (LBool.t,LBool.t,LBool.t,LBool.t) Tup4.t LOption.t
 with show,gmap;;

@type ground =
   (LBool.ground,LBool.ground,LBool.ground,LBool.ground) Tup4.ground LOption.ground
 with show,gmap;;

@type logic =
   (LBool.logic,LBool.logic,LBool.logic,LBool.logic) Tup4.logic LOption.logic
 with show,gmap;;

type groundi =
  ((LBool.ground,LBool.ground,LBool.ground,LBool.ground) Tup4.ground,
   (LBool.logic, LBool.logic, LBool.logic, LBool.logic)  Tup4.logic)
    LOption.groundi;;

let reify : VarEnv.t -> groundi -> logic = fun h x ->
  LOption.reify (Tup4.reify LBool.reify LBool.reify LBool.reify LBool.reify) h x;;


let inito : groundi = ocanren { LOption.None };;
