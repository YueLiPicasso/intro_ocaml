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
      d <  p' & fl == None }} ;;


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


(* capacity of the tank *)


let maximum_capacity = nat 5;;


(* One-step state transition of the jeep *)
(*

let step pre_state move post_state =
  let open Nat in
  ocanren {
    fresh pos, fuel, dumps in
      pre_state == (pos, fuel, dumps)  &
      {
        fresh d, pos', fuel' in
          move == Forward d            &
    post_state == (pos', fuel', dumps) &
             d <= fuel                 &
          (+) pos d pos'               &
          (+) fuel' d fuel

      | fresh d, pos', fuel' in
          move == Backward d           &
    post_state == (pos', fuel', dumps) &
             d <= fuel                 &
          (+) pos' d pos               &
          (+) fuel' d fuel

      | fresh q, fuel', dumps' in
          move == Unload q             &
          (* q <= maximum_capacity     & *)
             q <= fuel                 &
    post_state == (pos, fuel', dumps') &
          (+) q fuel' fuel             &
          { lookupo pos dumps None     &
            reseto pos q dumps dumps'  |
            fresh q', q'' in
          lookupo pos dumps (Some q')  &
          (+) q' q q''                 &
          reseto pos q'' dumps dumps'}
  

      } }
*)
