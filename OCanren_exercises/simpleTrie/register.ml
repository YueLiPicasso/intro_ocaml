(** The type of a register *)
open Logic;;
open LinearTuples;;


@type t =
   (Cell.t, Cell.t, Cell.t, Cell.t, Cell.t) Tup5.t
 with show, gmap;;

@type ground =
   (Cell.ground, Cell.ground, Cell.ground, Cell.ground, Cell.ground) Tup5.ground
 with show, gmap;;

@type logic =
   (Cell.logic, Cell.logic, Cell.logic, Cell.logic, Cell.logic) Tup5.logic
 with show, gmap;;

type groundi =
  (Cell.ground, Cell.logic,
   Cell.ground, Cell.logic,
   Cell.ground, Cell.logic,
   Cell.ground, Cell.logic,
   Cell.ground, Cell.logic) Tup5.groundi;;

let reify : VarEnv.t -> groundi -> logic = fun h x ->
  Tup5.reify Cell.reify Cell.reify Cell.reify Cell.reify Cell.reify h x;;
