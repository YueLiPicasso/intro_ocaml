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


(* Set L as an alias of the OCaml standard library List *)


module L = List;;


(* Provide short names for items of the OCanren module, namely,  
   - items from Logic.mli, Core.mli;
   - the Stream module path,
   - the Std module path, for OCanren standrd library  *)


open OCanren;;


(* Provide short names for items of the OCanren.Std sub-module, e.g., 
   - logical-data libraries Pair, Nat, Option, Bool and List, the 
     last three of which override OCaml's standard libraries of
     the same names;
   - logical list constructors (%), (%<), (!<) and nil;
   - generators/converters of logical-data.  *)


open OCanren.Std;;


(* We use the 'ocanren {...}'  construct to push down types to 
   logic-level. The type expressions expanded into can be found 
   near the definitions. The 'ocanren {...}' construct does not 
   recur through type abbreviations. Possible mistakes are high-
   lighted with the 'bad' type constructors. *) 
   

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
(* type movso      = move OCanren.logic GT.list OCanren.logic 
   type movso_bad  = moves OCanren.logic 
                   = move GT.list OCanren.logic
   type movos      = movo GT.list
                   = move OCanren.logic GT.list
   type movso'     = movos OCanren.logic
                   = move OCanren.logic GT.list OCanren.logic *)


@type moves  = move GT.list with show;;
@type movso  = ocanren { move GT.list } with show;;
@type movso_bad = ocanren { moves } with show;;
@type movos  = movo GT.list with show;;
@type movso' = ocanren { movos } with show;;


(* verify the type equality movso = movso' *)


module Mvo = struct
  let f  = fun x -> (x : movso  :> movso');;
  let f' = fun x -> (x : movso' :> movso );;
end;;


(* mini fuel dumps: where they are 
   and how much fuel each has *)
(* type dumpo   = (pos logic, fuel logic) Pair.logic
   type dumpos  = dumpo GT.list
                = (pos logic, fuel logic) Pair.logic GT.list
   type dumpso  = (pos logic, fuel logic) Pair.logic GT.list logic 
   type dumpso' = dumpos logic
                = (pos logic, fuel logic) Pair.logic GT.list logic  *)

@type dump   = pos * fuel with show;;
@type dumpo  = ocanren { pos * fuel } with show;;
@type dumpos = dumpo GT.list with show;;


@type dumps   = (pos * fuel) GT.list with show;;
@type dumpso  = ocanren { (pos * fuel) GT.list } with show;;
@type dumpso' = ocanren { dumpos } with show;;


module Dump = struct
  let f  = fun x -> (x : dumpso  :> dumpso' );;
  let f' = fun x -> (x : dumpso' :> dumpso  );;
end;;


(* the state of the jeep: where it is, how much fuel 
   it has in the tank, and what dumps it has now *)
(* type stato 
   = (pos logic, 
      (fuel logic, 
        (pos logic, fuel logic) Pair.logic GT.list logic)
      Pair.logic)
     Pair.logic
   = (pos logic, (fuel logic, dumpso) Pair.logic) Pair.logic
                              ^^^^^^ 

   type stato' 
   = (pos logic,
      (fuel logic, dumpos logic) Pair.logic)
     Pair.logic
   = (pos logic,
      (fuel logic, 
        (pos logic, fuel logic) Pair.logic GT.list logic) 
      Pair.logic)
     Pair.logic
   = stato


   type stato_bad 
   = (pos logic, (fuel logic, dumpso logic) Pair.logic) Pair.logic
                              ^^^^^^^^^^^^                          *)


@type state     = pos * fuel * dumps with show;;
@type stato     = ocanren { pos * fuel * (pos * fuel) GT.list } with show;;
@type stato'    = ocanren { pos * fuel * dumpos } with show;;
@type stato_bad = ocanren { pos * fuel * dumpso } with show;;


module Stato = struct
  let f  = fun x -> (x : stato  :> stato' );;
  let f' = fun x -> (x : stato' :> stato  );;
end;;


(* construct groundi/injected-level data *)
(* The unary operator (!!) comes from OCanren/Logic 

   val forward  : pos  -> (move, move OCanren.logic) OCanren.injected
   val backward : pos  -> (move, move OCanren.logic) OCanren.injected
   val unload   : fuel -> (move, move OCanren.logic) OCanren.injected
   val fill     : fuel -> (move, move OCanren.logic) OCanren.injected  *)


let forward x = !! (Forward x)
and backward x =  !! (Backward x)
and unload x = !! (Unload x)
and fill x = !! (Fill x);;

  




