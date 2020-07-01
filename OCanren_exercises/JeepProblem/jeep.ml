open GT;;

module L = List;;

open OCanren;;
open OCanren.Std;;


(* possible moves of the jeep *)
@type 'a move =
     Forward of 'a
   | Backward of 'a
   | Unload of 'a
   | Fill of 'a
 with show, gmap;;

(* The keyword '@type' has the same semantics as the keyword 'type' 
   except that it additionally invokes the GT package to automatically
   generate useful functons on this type, such as 'show' for pretty-
   printing and 'gmap' for applying a function over the parameter type,
   for instance, gmap : ('a -> 'b) -> 'a move -> 'b move, just like the
   'map' function from the OCaml standard library List. *)

