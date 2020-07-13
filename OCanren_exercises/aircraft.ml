open GT;;
open OCanren;;
open OCanren.Std;;

@type fuel = int with show;;
@type pos  = int with show;;
@type id   = int with show;;

(* the state of the fleet *)
(* all active aircrafts share the same position*)
@type state = pos * fuel GT.list with show;;


@type ('pos, 'fuel, 'id) action =
     Forward of 'pos
   | Transfer of 'fuel * 'id * 'id
   | Abandon of 'id
 with show, gmap;;


let forward x = ();;
let transfer x = ();;
let abandon x = ();;


(* substract fuel amount fu from the entry id of the fuel list flist, resulting in flist' *)
let subtract id fu flist flist' =
  ocanren {};;

let subtract_all (**)

let min_fuel (**)

(* capacity of each aircraft *)
let tank_capacity = nat 5;;

let step pre_state action post_state =
  ocanren {
    fresh p, l in (* p: position; l:fuel list *)
      pre_state == (p, l)     &
       { fresh d, p' fu, l' in (* d: distance forward; p': position after forward; fu: minimum fuel in the fuel list *)
          action == Forward d &
          d <= fu             &
          d <= tank_capacity  &
          (+) d p p'          &
          subtract  
          post_state == (p', l')

        | fresh fu, a1, a2 in (* fu: amount transferred; a1,a2: from aircraft a1 to a2 *)
           action == Transfer fu a1 a2 &
           select l a1 fu &

       }
  };;

