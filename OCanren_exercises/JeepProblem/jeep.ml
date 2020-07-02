open GT;;


(* The keyword '@type' has the same semantics as the keyword 'type' 
   except that it additionally invokes the GT package to automatically
   generate useful functons for the defined type, such as 'show' for 
   pretty-printing and 'gmap' for applying a function over the parameter
   type. For instance,

   @type 'a  <typeconstr> = ... with gmap;;

   creates  gmap : ('a -> 'b) -> 'a <typeconstr> -> 'b <typeconstr>, 
   just like the 'map' function from the OCaml standard library List if
   <typeconstr> is 'list'. The auto-generated functions and their types 
   can be observed using the -i option of the OCaml compiler *)


module L = List;;
open OCanren;;
open OCanren.Std;;


(* We use the 'ocanren {...}'  construct to push down types to 
   logic-level. The types expanded into can be found near the
   definitions *) 
   


(* we use (postive) intergers to count units.
   Use 'unitt' rather than 'unit', for the latter 
   is a built-in type *)


@type unitt = int with show;;


(* position of the jeep or a fuel dump, in units of 
   distance from base A, or units of position change
   of the jeep *)


@type pos = unitt with show;;


(* amount of fuel in the tank or a fuel dump, measured 
   in units of fuel, or units of fuel change *)


@type fuel = unitt with show;;


(* possible moves of the jeep *)
(* type movo = move OCanren.logic *)

@type move =
     Forward of pos
   | Backward of pos
   | Unload of fuel
   | Fill of fuel
 with show;;
@type movo = ocanren { move } with show;;


(* sequence of moves *)
(* type movso = move OCanren.logic GT.list OCanren.logic 
   type movso_bad = moves OCanren.logic 
   type movos = movo GT.list
   type movso' = movos OCanren.logic *)


@type moves  = move GT.list with show;;
@type movso  = ocanren { move GT.list } with show;;
@type movso_bad = ocanren { moves } with show;;
@type movos  = movo GT.list with show;;
@type movso' = ocanren { movos } with show;;


module Mvo = struct
  let f  = fun x -> (x : movso  :> movso');;
  let f' = fun x -> (x : movso' :> movso );;
end;;



(* mini fuel dumps: where they are 
   and how much fuel each has *)


@type dumps = (pos * fuel) GT.list with show;;


(* the state of the jeep: where it is, how much fuel 
   it has in the tank, and what dumps it has now *)


@type state = pos * fuel * dumps with show;;
@type stato = ocanren { pos * fuel * dumps } with show;;
@type stato_bad = ocanren { state } with show;;

(* *)


