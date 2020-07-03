(* From Dmitri Boulytchev *)

open GT
open OCanren
open OCanren.Std

@type rod   = A | B | C with show
@type move  = ocanren {rod * rod} with show
@type moves = ocanren {move list} with show                                                                  
@type state = ocanren {int list * int list * int list} with show

let a () = !! A
let b () = !! B
let c () = !! C
                                                          
let take_from state from_rod ring state' =
  ocanren {
    fresh rx, ry, others in      
      from_rod == A & state == (ring :: others, rx, ry) & state' == (others, rx, ry) |
      from_rod == B & state == (rx, ring :: others, ry) & state' == (rx, others, ry) |
      from_rod == C & state == (rx, ry, ring :: others) & state' == (rx, ry, others)
  }
  
let put_to state ring to_rod state' = success
  
let evalo_move state move state' =
  ocanren {
    fresh from_rod, to_rod, ring, state'' in     
     (from_rod, to_rod) == move                &
     from_rod =/= to_rod                       & 
     take_from state   from_rod ring   state'' &
     put_to    state'' ring     to_rod state'    
  }

let rec evalo state moves state' =
  ocanren {
   moves == [] & state == state' |
   fresh move, moves', state'' in
     move :: moves' == moves           &
     evalo_move state   move   state'' &
     evalo      state'' moves' state'
  }

                                                                                                     

