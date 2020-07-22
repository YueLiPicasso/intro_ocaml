open GT;;
open OCanren;;
open OCanren.Std;;

module Rat = LRational;;

(* injection primitive *)
let inj_int_ratio (x,y) =  match Rat.of_int_ratio (x,y) with a,b -> Rat.to_rat (a,b);; 

@type mine = A | B with show;;

module Machine = struct
  (* machine performance on A *)
  let p = inj_int_ratio (1,5)
  and r = inj_int_ratio (1,2);;

  (* machine performance on B *)
  let q = inj_int_ratio (3,10)
  and s = inj_int_ratio (33,100);;
end;;

module Mine = struct
  let a = !!A         (* injected *)
  and b = !!B;;
  
  let x = nat 100     (* init amount in A *)
  and y = nat 120;;   (* init amount in B *)
end;;


(* probability of success *)
let success m pr =
  ocanren {
    m == Mine.a & pr == Machine.p |
    m == Mine.b & pr == Machine.q
  };;

let fraction m fr =
  ocanren {
    m == Mine.a & fr == Machine.r |
    m == Mine.b & fr == Machine.s
  };;

let rec expectation amt_A amt_B plan (expc : Rat.groundi) =
  ocanren {
    plan == [] & expc == (0,1) |
    fresh m, ms, ems, pr, fr,
          amt_now, amt_mined, amt_left in
      plan == m :: ms                  &
      { m == Mine.a & amt_A == amt_now |
        m == Mine.b & amt_B == amt_now }
      & success m pr  & fraction m fr  &
      Rat.( * ) fr amt_now amt_mined   &
      Rat.( + ) amt_mined amt_left amt_now
};;


(* Unfinished *)
