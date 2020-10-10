(** The type of a cell in a register *)
open OCanren;;
open OCanren.Std;;
open Constant;;


@type ground = Constant.ground  Option.ground with show,gmap;;
@type logic = Constant.logic Option.logic     with show,gmap;;
type groundi = (Constant.ground, Constant.logic) Option.groundi;;

let reify = fun h x -> Option.reify Constant.reify h x;;
