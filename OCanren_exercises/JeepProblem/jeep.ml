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


@type unitt = int with show;;
@type pos = unitt with show;;
@type fuel = unitt with show;;
@type ('a, 'b) move =
       Forward of 'a        (* The type 'pos'  is intended for 'a *)
     | Backward of 'a
     | Unload of 'b         (* The type 'fuel' is intended for 'b *)
     | Fill of 'b
 with show, gmap;;


module MoveLogic : sig
  
  val forward :
    ('a, 'b) injected ->
    (('a, 'c) move, ('b, 'd) move logic) injected;;
  val backward :
    ('a, 'b) injected ->
    (('a, 'c) move, ('b, 'd) move logic) injected;;
  val unload :
    ('c, 'd) injected ->
    (('a, 'c) move, ('b, 'd) move logic) injected;;
  val fill :
    ('c, 'd) injected ->
    (('a, 'c) move, ('b, 'd) move logic) injected;;
      
end = struct
  
  module T = struct
    type ('a, 'b) t = ('a, 'b) move;;
    let fmap = fun x -> gmap(move) x;;
    (* let fmap = gmap(move);; *)     (* problem caused by value restriction *)
  end;;

  module Fmove = Fmap2(T);;

  let di x = inj @@ Fmove.distrib x;;
  
  let forward x =  di (Forward x)
  and backward x =  di (Backward x)
  and unload x = di (Unload x)
  and fill x = di (Fill x);;

end;;



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
     - if p is after p_1 (p > p_1), then reset p to fl in l_res to get ln_res, 
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


(* Definition of ocanren {.. }? This construct performs translation where, among others,
   value constructor applications are detected and all constructors are converted to lower 
   case. For example, 'Forward d' in the definition of 'step' is converted into 'forward d'.
   Therefore we locally open the module MoveLogic to provide such a function 'forward'.  *)


let step pre_state move post_state =
  let open Nat in
  let open MoveLogic in  (* exports invoked implicitly *)              
  ocanren {
    fresh pos, fuel, dumps in
      pre_state == (pos, fuel, dumps)  &
      {
        fresh d, pos', fuel' in          (* d: distance moved; pos': new position; fuel': new fuel level *)
          move == Forward d            & (* confirm the kind of move: forward *)
             d <= maximum_capacity     & (* cannot move more than the tank's capacity  *)
             d <= fuel                 & (* cannot move more than allowed by the actual fuel level *)
           (+) pos   d pos'            & (* compute new position *)
           (+) fuel' d fuel            & (* compute new fuel level *)
    post_state == (pos', fuel', dumps)   (* the dumps list does not change in the new state *)

      | fresh d, pos', fuel' in          (* d: distance moved; pos': new position; fuel': new fuel level *)
          move == Backward d           & (* confirm the kind of move: backward *)
             d <= maximum_capacity     & (* cannot move more than the tank's capacity  *)
             d <= fuel                 & (* cannot move more than allowed by the actual fuel level *)
           (+) pos'  d pos             & (* compute new position *)
           (+) fuel' d fuel            & (* compute new fuel level *)
    post_state == (pos', fuel', dumps)   (* the dumps list does not change in the new state *)

      | fresh q, fuel', dumps' in        (* q: fuel unloaded; fuel': new fuel level; dumps': new dumps configuration *)
          move == Unload q             & (* confirm the kind of move: unload fuel *)
             q <= maximum_capacity     & (* cannot unload more than the capacity *)
             q <= fuel                 & (* cannot unload more than the actual fuel level *)
           (+) q fuel' fuel            & (* compute new fuel level *)
          { lookupo pos dumps None     & (* if there is no dump here yet *)
            reseto  pos q dumps dumps'   (* create a new dump with q, updating the dumps configuration *)
          |                              (* or *)
            fresh q', q'' in             (* q': fuel in the existing dump here; q'': updated fuel in the dump *)
          lookupo pos dumps (Some q')  & (* if there is already a dump here with fuel q' *)
          (+) q' q q''                 & (* compute the dump's fuel q'' after unloading *)
          reseto pos q'' dumps dumps'} & (* update the dumps configuration *)
    post_state == (pos, fuel', dumps')   (* only the position does not change in the new state *) 
  
      | fresh q, fuel', q', q'', dumps' in
          move == Fill q               &
             q <= maximum_capacity     &
         { pos == 0                    & (* fill at the base *)
    post_state == (pos, fuel',dumps)   &
           (+) fuel q fuel'            &
         fuel' <= maximum_capacity     |

         positive pos                  & (* fill on the way *)
    post_state == (pos, fuel', dumps') &
    lookupo pos dumps (Some q')        &
             q <= q'                   & (* fill no more than what the dump has  *)
         (+) fuel q fuel'              & (* update fuel in the tank*)
         fuel' <= maximum_capacity     & (* no more fuel in the tank tha its capacity *)
         (+) q'' q q'                  & (* compute remaining fuel q'' of the dump *)
         reseto pos q'' dumps dumps' } } }

(* The inferred type for the parameter 'pre_state' is:

(  ( Nat.ground, 
     (Nat.ground, (Nat.ground, Nat.ground) Pair.ground List.ground) Pair.ground
   ) Pair.ground,

   (Nat.logic *
    (Nat.logic, (Nat.logic, Nat.logic) Pair.logic List.logic) Pair.logic) Logic.logic

)  Logic.injected 


   Why the * , not Pair.logic ?

   Not sure, but they are compatible. 
   See LPair.mli from the OCanren standard library. *)



(* Where I'm now*)
(* tree, sorting, jeep--half way  *)

(* next *)
(* jeep, WGC, Hanoi, aircraft range? *)

(* capacity issue: permute list of length 8 maximun *)
(* Paper: mproving Refutational Completeness of Relational Search via Divergence Test  *)

(* APLAS, miniKanren workshop ICFP ---online  *)
