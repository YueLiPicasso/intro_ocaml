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

1. Given a mining plan in terms of a sequence of mining sites, 
   e.g., [A;A;B;B;A;B], to compute the expectation of this plan.

1. Given  an interval, we ask the program to find
all mining plans that yield an (rational number) expectation that is located in this interval.

## Reference

Bellman, Richard Ernest, _The Theory of Dynamic Programming_, Santa Monica, Calif.: RAND Corporation, P-550, 1954. As of July 20, 2020: https://www.rand.org/pubs/papers/P550.html

## Compilation

First run `make` in the [LRational](LRational) directory. This creates the compiled interface and implementation of the
`LRational` library. Then run `make` in the top level directory of the Gold Mining project.

## Review

Here are some observations about the program.

### Test 1

For this test I used the following specifications. 

```ocaml
module Machine' = struct
  open Rat;;
  (* machine performance on A *)
  let p = inj_int_ratio (1,5)
  and r = inj_int_ratio (1,2);;
  (* machine performance on B *)
  let q = inj_int_ratio (3,10)
  and s = inj_int_ratio (33,100);;
end;;

module Mine' = struct
  let x = inj_int_ratio (100,1)     (* init amount in A *)
  and y = inj_int_ratio (120,1);;   (* init amount in B *)
end;;

```

Only for trivial queries, like "what is the
expectation for plan [A] (or [B]) ?", can it give an answer in a short moment. For even slightly
larger a query like "what is the expectation for plan [A;B] ?", it looks as if the program
diverges. The reason for this, I suppose, is with the primitive rational number arithmetic.
It does not perform simplification so that the numerators and denominators quickly get huge.
For instance, the expectation for mining at B for once, as computed by the program,
is (11880 , 1000), i.e, 11880 divided by 1000. It might be helpful if somehow this
number is simplified to (297, 25).

Given the expectation (11880, 1000), the program can correctly point out that the plan that has
this value is just [B]. I did not test the reverse behaviour further.  


I also hoped that the program should be able to compute a list of plan-expectation pairs, i.e.,
enumerate all possible plans and show the corresponding expectation. Asking the program to do so,
however, resulted in long waiting time without interesting results being returned.    


### Test 2

This time I tried to use smaller numbers.

```ocaml
module Machine = struct
  open Rat;;
  (* machine performance on A *)
  let p = inj_int_ratio (1,3)
  and r = inj_int_ratio (1,2);;
  (* machine performance on B *)
  let q = inj_int_ratio (1,2)
  and s = inj_int_ratio (1,3);;
end;;

module Mine = struct
  open Rat;;
  let x = inj_int_ratio (1,1)     (* init amount in A *)
  and y = inj_int_ratio (2,1);;   (* init amount in B *)
end;;
```

The program did marginally better, for instance, it can tell the expectation for [A;B] or
[B;A]. Unfortunately and curiously, for [A;A] and [B;B], and larger plans , it did not return
any answer before I waited for too long and killed the process.

### Summary

In general, the programm is not efficient at all. All expected tasks cannot be done in a
non-trivial manner. A lot is there to be improved. A better rational number arithmetic
package is perhaps the key, which in turn requires a well understood and well-behaved
natural number arithmetic package.