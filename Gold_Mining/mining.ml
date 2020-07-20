open GT;;

(* We possess two gold mines: Anaconda and Bonanza, 
   represented by A and B respectively.  *)

@type mine = A | B with show;;

(* We also have a gold-mining machine with the following 
   characteristics: 

   Used in Anaconda (resp. Bonanza), the machine will mine, with 
   probability p (resp. q), a fixed fraction r (resp. s) of the 
   gold there and be undamaged; with probability (1-p) (resp. (1-q)) 
   it will mine nothing and be damaged beyond repair. 

   Initially the amount in each mine is x and y respectively. *)

let p = 0.2 and r = 0.50 and x = 100.0 ;;
let q = 0.3 and s = 0.33 and y = 120.0;;

(* Use rational numbers instead of floating point *)



(* At any stage, as long as the machine is undamaged, we have our choice 
   of using the machine in Anaconda or Bonanza. 

   Given a mining plan in terms of a sequence of mining sites, 
   e.g., [A;A;B;B;A;B], we compute the expectation of this plan. *)


let rec expectation plan =
  match plan with
  | [] -> 0.0
  | []



(* Start in A : 
  one possibility : rx -> r(1-r)x -> ...  *)
