open GT;;
module L = Stdlib.List;;
open OCanren;;
open OCanren.Std;;


@type unitt = int with show;;

@type fuel = unitt with show;;

(* position of the fleet *)

@type pos  = unitt with show;;

(* list the amount of fuel in each aircraft of the fleet *)

@type fuel_profile = fuel GT.list with show;;

(* the state of the fleet *)

@type state = pos * fuel_profile with show;;

@type ('pos, 'fuel_profile) action =
     Forward of 'pos
   | Abandon of 'fuel_profile (* post-abandoning profile  *)
 with show, gmap;;

(* data injection primitives *)

module Action : sig
  val forward :
    ('a, 'b) injected ->
    (('a, 'c) action, ('b, 'd) action logic) injected
  val abandon :
    ('a, 'b) injected ->
    (('c, 'a) action, ('d, 'b) action logic) injected
end = struct

  module T = struct
    type ('a,'b) t = ('a,'b) action;;
    let fmap = fun x -> gmap(action) x;;
  end;;
  
  module F = Fmap2(T);;

  let distrib_inj x = inj @@ F.distrib x;;

  let forward x = distrib_inj (Forward x);;
  let abandon x = distrib_inj (Abandon x);;
 
end;;

(* substract an amount amt from all members of fl, resulting in fl' *)

let rec subtract_all amt fl fl' =
  let open Nat in 
  ocanren {
    fl == [] & fl' == [] |
    fresh h, t, h', t' in
     fl == h :: t      &
     fl' == h' :: t'   &
     (+) amt h' h      &
     subtract_all amt t t'
};;

(* capacity of each aircraft *)

let tank_capacity = nat 5;;

(* Some amount amt of fuel is given to and shared among the fleet, 
   changing the fleet's fuel profile from fl to fl'. *)

let rec share_fuel amt fl fl' =
  let open Nat in
  ocanren {
    fl == [] & fl' == []   |
    fresh amta, amtb in
     (+) amta amtb amt     &
      fresh h, t, h',t' in
       fl  == h  :: t      &
       fl' == h' :: t'     &
       (+) h amta h'       &
       h' <= tank_capacity &
       share_fuel amtb t t'
};;

(* abandon one aircraft and transfer its fuel to the rest 
   of the fleet *)

let abandono fl fl' =
  ocanren {
    fl == [] & fl' == [] |
    fresh h, t in
     fl == h :: t        &
     share_fuel h t fl'
};;


(* postive natural number *)

let positive x = ocanren { fresh n in x == Nat.succ n };;


(* single step state transition  *)

let step pre_state action post_state =
  let open Action in
  let open Nat in
  ocanren {
    fresh p, l in (* p: position; l:fuel profile *)
      pre_state == (p, l)      &
      { fresh d    ,   (* d    : distance forward *)
              p'   ,   (* p'   : position after forward *)
              l'       (* l'   : updated fuel list *)
        in 
          action == Forward d  &
          positive d           &
          d <= tank_capacity   &
          (+) d p p'           &
          subtract_all d l l'  &  
          post_state == (p', l')

        | fresh l' in (* updated fuel profile *)
           action == Abandon l' &
           abandono l l' &
           post_state == (p, l')
       }
  };;


