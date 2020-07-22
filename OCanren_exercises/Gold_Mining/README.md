# Stochastic Gold Mining


We possess two gold mines: Anaconda and Bonanza, 
   represented by A and B respectively. We also have a gold-mining machine with the following 
   characteristics: 

   Used in Anaconda (resp. Bonanza), the machine will mine, with 
   probability p (resp. q), a fixed fraction r (resp. s) of the 
   gold there and be undamaged; with probability (1-p) (resp. (1-q)) 
   it will mine nothing and be damaged beyond repair. 

   Initially the amount in each mine is x and y respectively. At any stage, as long
   as the machine is undamaged, we have our choice 
   of using the machine in Anaconda or Bonanza. 

## What we want OCanren to do

1.   Given a mining plan in terms of a sequence of mining sites, 
   e.g., [A;A;B;B;A;B], to compute the expectation of this plan.

1. Given an expectation, to compute a mining plan that satisfies it.

## Reference

Bellman, Richard Ernest, _The Theory of Dynamic Programming_, Santa Monica, Calif.: RAND Corporation, P-550, 1954. As of July 20, 2020: https://www.rand.org/pubs/papers/P550.html

## Compilation

First run `make` in the [LRational](LRational) directory. This creates the compiled interface and implementation of the
`LRational` library. Then run `make` in the top level directory of the Gold Mining project.   