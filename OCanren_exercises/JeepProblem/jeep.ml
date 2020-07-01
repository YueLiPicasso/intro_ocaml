(* 

The Jeep Problem 

https://mathworld.wolfram.com/JeepProblem.html 

A jeep is going to cross a desert: moving from base A on one side
of the desert to base B on the other side. There is no
fuel station along the way. The jeep has one fuel tank, and it 
carries empty bottles:  on the way it can 
transfer some fuel from the tank to some bottles and leave those 
bottles there. The jeep may go back to base A to refuel, or refuel 
in the desert from previoulsy left bottles.


We abstract from particular details of the jeep. For instance,  the jeep's 
tank may actually hold 4.67 liters of fuel maximum  and with which it 
can run 412.6 km. We take 4.67 liters as one unit of fuel
and take 412.6 km as one unit of distance, and say that the jeep's 
tank holds one unit of fuel, and with which it can run for one unit of 
distance.


The total distance of the journey, if we want the problem to be non-trivial,
shall be longer than one unit, so that the jeep must unload fuel on the way,
i.e.,  creating some fuel dump sites in the desert, and refuel from both
base A and the fuel dump sites in order to reach base B.  

*)


open GT;;

module L = List;;

open OCanren;;
open OCanren.Std;;


(* unit move *)
@type 'a move =
     Forward of 'a
   | Backward of 'a
   | Unload of 'a
   | Fill of 
