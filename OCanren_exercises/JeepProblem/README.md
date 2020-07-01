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

## Abstraction and Simplification

We could abstract from the fuel tank's details.

For some arbitrary positive interger C, we take (L/C) liters as one unit of fuel
and take (D/C) miles as one unit of distance, and say that the jeep's tank has a
maximum capacity of C units of fuel, and with which it can run for C units of
distance at the most.

Moreover, we assume that the jeep consumes fuel and covers distance in a discrete
(or unit-by-unit) and propotional manner, where one unit of fuel is consumed at
a time,  resulting in one unit of distance traveled.

Transfer of fuel between the tank and bottles is also discrete:  only whole units
of fuel is possible. For example, if the jeep has 3 units of fuel left in the tank
that has a capacity of 5 units, then the ony possible refuel amounts are 1 unit or
2 units, and the only possible amount of dumped fuel are 1 unit, 2 units, or 3 units. 

These are all assumed to be done unit-by-unit.

A, B --- bases
L --- maximum fuel capacity of the jeep, in liters 
D --- maximum distance capacity by the jeep, in miles
C --- abstract fuel/distance capacity of the jeep, in units

## Reference

https://mathworld.wolfram.com/JeepProblem.html 