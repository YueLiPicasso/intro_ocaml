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
    l == []            &
    {
      fl == 0        &
      ln == []
      |
      positive fl    &
      ln == [(p,fl)]
    }
    |
    fresh p_1, fl_1, l_res, ln_res in
      l == (p_1,fl_1)::l_res &
      { { p == p_1 & { fl == 0 & ln == l_res | positive fl & ln == (p,fl)::l_res} } |
        { p >  p_1 & reseto p fl l_res ln_res & ln == (p_1,fl_1)::ln_res }          |
        { p <  p_1 & {fl == 0 & l == ln | positive fl & ln == (p,fl)::l } }
      } }


(* capacity of the tank, or how many units we divide the tank into *)


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
        fresh d, pos', fuel'   in        (* d: distance moved; pos': new position; fuel': new fuel level *)
          move == Forward d            & (* confirm the kind of move: forward *)
             d <= maximum_capacity     & (* cannot move more than the tank's capacity  *)
             d <= fuel                 & (* cannot move more than allowed by the actual fuel level *)
           (+) pos   d pos'            & (* compute new position *)
           (+) fuel' d fuel            & (* compute new fuel level *)
    post_state == (pos', fuel', dumps)   (* the dumps list does not change in the new state *)

      | fresh d, pos', fuel'   in        (* d: distance moved; pos': new position; fuel': new fuel level *)
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
  
       | fresh q, fuel'        in        (* q: fuel added; fuel': new fuel level *)
          move == Fill q               & (* confirm the kind of move: fuel the jeep *)
             q <= maximum_capacity     & (* cannot add more fuel than the capacity *)
         fuel' <= maximum_capacity     & (* new fuel level cannot exceed the capacity *)
         { pos == 0                    & (* if fill at the base *)
           (+) fuel q fuel'            & (* compute the new fuel level *)
    post_state == (pos, fuel',dumps)     (* only the fuel level changes in the new state *)
         |                               (* or *)
           positive pos                & (* if fill on the way *)
           fresh q',q'',                 (* q': fuel of the dump before filling; q'': fuel of the dump after filling *)
                 dumps' in               (* dumps': new dumps configuration *)
    lookupo pos dumps (Some q')        & (* there is a dump here with fuel q' *)
             q <= q'                   & (* use no more than what the dump has *)
           (+) fuel q fuel'            & (* update fuel in the tank*)
           (+) q'' q q'                & (* compute remaining fuel q'' of the dump *)
    reseto pos q'' dumps dumps'        & (* update the dumps configuration *)
    post_state == (pos, fuel', dumps')   (* only the position does not change in the new state *)
} } }


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


module Kind : sig

  
  (* numeric encoding of non-zero moves. 
     forward/backward: 0 
     unload/fill:      1   *)


  val kind :
    ((Nat.ground, Nat.ground) move,
     (Nat.logic,  Nat.logic) move logic) injected
    -> (int, int logic) injected -> goal ;;

  
end = struct

  
  (* Use !(...) to locally turn off ocanren syntax extension: in this module
     it seems just a matter of taste to use kind instead of kind' *)

  
  let kind move code =
    let open MoveLogic in
    ocanren {
      fresh x , y in
      { move == Forward x | move == Backward x} & x =/= 0 & code == !(!!0)
    | { move == Unload y  | move == Fill y } & y =/= 0 &  code == !(!!1)
    };; (* 'code' has type (int, int Logic.logic) Logic.injected  *)


  let kind' move code =
    let open MoveLogic in
    ocanren {
      fresh x , y in
      { move == Forward x | move == Backward x} & x =/= 0 & code == 0
    | { move == Unload y  | move == Fill y } & y =/= 0 &  code == 1
    };; (* 'code' has type (Nat.ground, Nat.logic Nat.t Logic.logic) Logic.injected 
      which is compatible with (Nat.ground, Nat.logic) Logic.injected *)


   (* simple comparison *)


   let one = ocanren { 1 }
   and one' = ocanren { !(1) }
   and one'' = ocanren { !( !!1 ) }
   and one''' = !!1;;


(* val one    : OCanren.Std.Nat.groundi 
              = (Nat.ground, Nat.logic) injected
   val one'   : int
   val one''  : (int, int logic) injected
   val one''' : (int, int logic) injected


   Note that Nat.ground is  the type for peano numbers *)


end;;


module Steps = struct

  open Kind;;

  
  let f = Tabling.(tabledrec four) (* use tabling to avoid looping *)
      (fun f k pre_state moves post_state ->
         ocanren {
           moves == [] & pre_state == post_state |
           fresh mid_state, m, mres, k_m in
             moves == m :: mres                  &
             kind m k_m                          &
             k_m =/= k                           & (* no conseqcutive moves of the same kind *)
             step pre_state m mid_state          &
             f k_m mid_state mres post_state  })
end;;


let steps pre_state moves post_state =
  Steps.f !!2 pre_state moves post_state;;


(* project from logic domain to function domain *)


let prj_moves x = List.to_list (gmap(move) Nat.to_int Nat.to_int) (project x);;
let prj_state st =
  let x,(y,z) = project st in
  (Nat.to_int x, Nat.to_int y,
   List.to_list (fun (a,b) -> Nat.to_int a, Nat.to_int b) z );;


(* initial state *)


let init = pair (nat 0) @@ pair maximum_capacity (nil ());;


(* Exercises: 
   1. Use the -i option of the ocamlc/ocamlopt compiler to print 
   the types of prj_moves and init; 
   2. Refer to LList.mli, Logic.mli, OCanren.ml etc to find out the types of
   to_list, project, to_int, pair, nat, etc;
   3. Deduce by hand the types in 1 using knowledge from 2.  *)


(* type shortcuts for pretty-printing purposes *)
(* Note that type constructor application binds tighter 
   than pair formation '*'. *)


@type moves = (pos, fuel) move GT.list with show;;
@type state = pos * fuel * (pos * fuel) GT.list with show;;



let open MoveLogic in
L.iter (fun q -> Printf.printf "State when reachinng 6: %s\n%!" @@ show(state) q) @@ Stream.take ~n:1 @@
run q (fun q -> ocanren {steps init [Forward 2;Unload 1;Backward 2;Fill 5;Forward 2;Fill 1;Forward 4] q}) prj_state;

L.iter (fun q -> Printf.printf "Make some dumps: %s\n%!" @@ show(state) q) @@ Stream.take ~n:1 @@
run q (fun q -> ocanren {steps init [Forward 1;Unload 2;Backward 1;Fill 4;Forward 2;Unload 1;Backward 2] q}) prj_state;

L.iter (fun q -> Printf.printf "Find moves given final state (2,2,[(1,2)]): %s\n%!" @@ show(moves) q) @@ Stream.take ~n:1 @@
run q (fun q -> ocanren {steps init q (2,2,[(1,2)])}) prj_moves;

(* we take 4, and see that the first is the optimal: it is the suffix of all other
     solutions, whose prefixes does nothing but wasting the fuel *)
L.iter (fun q -> Printf.printf "Find moves given final state (6,0,[]): %s\n%!" @@ show(moves) q) @@ Stream.take ~n:4 @@
run q (fun q -> ocanren {steps init q (6,0,[])}) prj_moves;
 
(* This takes more time ! *)
L.iter (fun q -> Printf.printf "Find moves given final state (8,?,?): %s\n%!" @@ show(moves) q) @@ Stream.take ~n:1 @@
run q (fun q -> ocanren {fresh r, s in steps init q (8,r,s)}) prj_moves; 

let l = ocanren {
    [Forward (1); Unload (1); Backward (1); Fill (2); Forward (1);
     Unload (2); Backward (1); Fill (5); Forward (1); Unload (1);
     Backward (1); Fill (3); Forward (1); Fill (1); Forward (1);
     Unload (2); Backward (2); Fill (5); Forward (3); Unload (1);
     Backward (1); Fill (1); Backward (1); Fill (1); Backward (1);
     Fill (4); Forward (1); Fill (2); Forward (1); Fill (1);
     Forward (1); Fill (1); Forward (5)] } in 
L.iter (fun q -> Printf.printf "Find final state given moves: %s\n%!" @@ show(state) q) @@ Stream.take ~n:1 @@
run q (fun q -> ocanren { steps init !(l) q}) prj_state;;


(* Set maximum capacity to 6, and then : *)(*

let open MoveLogic in

(* This takes more time ! *)
   L.iter (fun q -> Printf.printf "Find moves given final state (9,?,?): %s\n%!" @@ show(moves) q) @@ Stream.take ~n:1 @@
   run q (fun q -> ocanren {fresh r, s in steps init q (9,r,s)}) prj_moves;

let l = ocanren {
    [Forward (2); Unload (1); Backward (2); Fill (5); Forward (1);
     Unload (3); Backward (1); Fill (5); Forward (3); Unload (1);
     Backward (2); Fill (1); Backward (1); Fill (5); Forward (1);
     Fill (2); Forward (1); Fill (1); Forward (1); Fill (1); Forward (6)] } in
L.iter (fun q -> Printf.printf "Find final state given moves: %s\n%!" @@ show(state) q) @@ Stream.take ~n:1 @@
run q (fun q -> ocanren { steps init !(l) q}) prj_state ;; 

*)

(* next *)
(* WGC, Hanoi, aircraft range? *)

(* capacity issue: permute list of length 8 maximun; 
   jeep problem 15 units of tank capacity for 23 units of distance: 
   process killed *)
