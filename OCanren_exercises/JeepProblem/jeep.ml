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


(* Given the position d of the jeep, see if there is any fuel dump 
   there, by searching for d in an association list l consisting of 
   <fuel dump position>-<dump fuel amount> pairs, the indices of 
   which are sorted in ascending order. If there is a dump, return
   the fuel amount fl there (wrapped in the 'Some' value constrcutor), 
   otheriwse return None. *)


let rec lookupo d l fl =
  let open Nat in
  ocanren {
    l == [] & fl == None |
    fresh p', fl', l' in
      l == (p', fl') :: l' &
    { d == p' & Some fl' == fl   |
      d >  p' & lookupo d l' fl |
      d <  p' & fl == None }}
;;

(* val lookupo : 
   Nat.groundi ->
  ((Nat.ground, 'a) Pair.ground, (Nat.logic, 'b) Pair.logic) List.groundi ->
  ('a Option.ground, 'b GT.option logic) injected ->
  OCanren.goal  

   Note that
 
   ((Nat.ground, 'a) Pair.ground, (Nat.logic, 'b) Pair.logic) List.groundi
   
   expands to
 
   ((Nat.ground, 'a) Pair.ground List.ground, 
   (Nat.logic, 'b) Pair.logic List.logic) injected 
   
   whose first parameter is a ground-level list of ground-level pairs of
   ground-level nats with 'a. *)


(* test if a number is positive *)


let positive n = ocanren { fresh x in n == Nat.succ x };;


(* Reset the fuel dump at p to fl units of fuel, thus turning
   an exsting ordered list l of dumps into a new list ln. 
   The rules are: 
   - if the existing list of dumps is empty, then:
     - if the new fuel amount is 0, then nothing is done;
     - if the new fuel amount is not 0 (positive), then creata a new dunp;
   - if the first existing dump is (p_1,fl_1), and the rest are l_res, 
     then:
     - if just this dump is to be reset (p == p_1), then:
       - if the new amount is 0, then this dump is simply removed from the list;
       - if the new amount is not 0 (positive), then set the fuel to this new amount
     - if p is after p_1 (p > p_1), then resett p to fl in l_res to get ln_res, 
       and ln is (p_1,fl_1)::ln_res;
     - if p is before p_1, then:
       - if the new fuel amount is 0, then nothing is done;
       - if the new fuel amount is not 0 (positive), then prefix a dump (p,fl) to l. 
*)


let rec reseto p fl l ln =
  let open Nat in
  ocanren {
    l == [] & { fl == 0 & ln == [] | positive fl & ln == [(p,fl)] } |
    fresh p_1, fl_1, l_res, ln_res in
l == (p_1,fl_1)::l_res &
{ { p == p_1 & { fl == 0 & ln == l_res | positive fl & ln == (p,fl)::l_res} } |
  { p >  p_1 & reseto p fl l_res ln_res & ln == (p_1,fl_1)::ln_res }          |
  { p <  p_1 & {fl == 0 & l == ln | positive fl & ln == (p,fl)::l } }
} }


