# The Jeep Problem 

We describe the general idea of the jeep problem, followed by introducing
some abstraction in order to formulate and solve it mathematically.

## Background

A jeep is going to cross a desert: traveling from base A on one side
of the desert to base B on the other side. 

The jeep has a fuel tank. Suppose that the tank holds L liters of fuel
maximum and with which it can run for D miles at the most.

There is a fuel station in base A, providing practically unlimited
fuel for the jeep.  However, the road linking bases A and B is much longer
than D miles, meaning that the jeep, fully fueled at base A, must refuel
somewhere in between the bases A and B in order to complete the journey.
Unfortunately there is no more fuel stations on the way, and at any time
the jeep can carry at the most one tank of fuel.

 
The strategy that the jeep uses to cross the desert
is that it carries empty bottles:  on the way it could transfer some fuel 
from the tank to some bottles and leave the bottles there, thus creating 
mini fuel dumps in the desert. The jeep could refuel either by going back to base 
A or from the mini fuel dumps.  


The questions are: how should the jeep create mini fuel dumps and 
refuel in order to complete the journey, and  what is the minimum amount of 
fuel needed ?   

## Abstraction

We could abstract from the jeep and its fuel tank's details.

### Discrete Motion

For some arbitrary positive interger C, we take (L/C) liters as one unit of fuel
and take (D/C) miles as one unit of distance, and say that the jeep's tank has a
maximum capacity of C units of fuel, and with which it can run for C units of
distance at the most.

Moreover, we assume that the jeep consumes fuel and covers distance in a discrete
(or unit-by-unit) and propotional manner, where one unit of fuel is consumed at
a time,  resulting in one unit of distance traveled.

### Discrete Fuel Trasnfer 

Transfer of fuel between the tank and bottles is also discrete:  only whole units
are allowed. For example, if the jeep has 3 units of fuel left in the tank
that has a capacity of 5 units, then it can only refuel (from base A or mini fuel dumps)
for 1 unit or 2 units, and can only transfer (to and from mini fuel dumps) for 1 unit, 2 units,
or 3 units.

### Notation

We summarize all mentioned notation in the following table. 


Letter | Meaning | Type | Unit
--- | --- | --- | ---
A, B | bases | N/A | N/A
L | maximum fuel capacity of the jeep | positive real | liter
D | maximum distance capacity of the jeep on L | positive real | mile
C | abstract fuel or distance capacity of the jeep | positive interger | unit

## Remarks

Although the value of the abstract capacity C is arbitrarily picked, we must set it
to at least 3. If we set C = 1 or 2, then it would be impossible for the jeep to reach
the destination, given our _discrete motion_ and _discrete fuel transfer_ assumptions.
For instance, C = 1 implies that the jeep would move forward for 1 unit of distance, 
runnning out of fuel and getting stuck.  C = 2 implies three possibilities: 1) moving
forward for 2 units of distance, runnning out of fuel and getting stuck; 2) moving
forward for 1 unit, deposit 1 unit (the remaining) fuel, and then get stuck; 3) moving
forward for 1 unit and backward for 1 unit, returning to base A and making no progress.

## Reference

https://mathworld.wolfram.com/JeepProblem.html 