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

## What we want the program to do

1. (Forward Run) Given a mining plan in terms of a sequence of mining sites,
   to compute the expectation of this plan. This amounts to building and then
   evaluating an arithmetic expression on (positive) rational numbers. For instance,
   the expectation for [A;A;B;B] is computed from
   
   ```
   p[rx + p[r(1-r)x + q[sy + q[s(1-s)y]]]]
   ```

1. (Backward Run) Given  an interval _[r1, r2]_, we ask the program to find
all mining plans that yield an  expectation _r_ that is located in this interval,
i.e.,  _r1 < r < r2_.

## What can the program actually do

1. (Forward Run) The current implementation can quickly compute the expectation of any given
plan _that contains no more than 4 mining sites_, like [A;B;B:A]. For larger plans the
 computation becomes very slow.

1. (Backward Run) Picking one expectation value and feed it to the program,
it can be found out which plan has this particular expectation. This is essentially a generate-and-test process,
 where the program enumerates in a certain order all possible plans and see which plan's expectation happens to
 equal the specified. 

1. (Generator) The program can  be used to generate a table of possible plans and
their corresponding expectation. The performance of this, unfortunately,
is also subject to limitation. Within a not very long time the current
implementation can enumerate the first 25 possible plans, covering some of the
cases that involve  0,1,2,3 or 4 mining sites, and an excerpt of which is like this:

```ocaml
([], (0, 1))
([A], (1, 6))
([B], (2, 6))
([A; B], (10, 36))
([B; A], (15, 36))
([A; A], (14, 72))    (* A pair like (14,72) denotes the rational number 14/72 *)
([B; B], (48, 108))     
([A; A; B], (100, 432))
([B; A; A], (186, 432))
([A; B; A], (126, 432))
([A; A; B; B], (1896, 7776))
([B; A; A; B], (3444, 7776))
([A; B; A; B], (2364, 7776))
([B; B; A; A], (3834, 7776))
```


## Compilation

First run `make` in the [LRational](LRational) directory. This creates the compiled interface and implementation of the
`LRational` library. Then run `make` in the top level directory of the Gold Mining project.

## Review and Future Work

The major function named `expectation` went through four versions, for we applied several techniques to make it
work in the current acceptable manner, which are as follows.

1. To distibute  existential variables (introduced by the `fresh` keyword) among disjuncts so that there isn't
any fresh variable whose scope covers an entire disjunction. This corresponds to the step from [expectation``](https://github.com/YueLiPicasso/intro_ocaml/blob/7496a7cd6968b56eba11c84affc04a20906acfdf/OCanren_exercises/Gold_Mining/mining.ml#L76) to
 [expectation```](https://github.com/YueLiPicasso/intro_ocaml/blob/7496a7cd6968b56eba11c84affc04a20906acfdf/OCanren_exercises/Gold_Mining/mining.ml#L99).  Note, however, that percolating existential variables through conjuncts does _not_ imporve the
 performance as much (from [expectation'''](https://github.com/YueLiPicasso/intro_ocaml/blob/7496a7cd6968b56eba11c84affc04a20906acfdf/OCanren_exercises/Gold_Mining/mining.ml#L99) to [expectation''''](https://github.com/YueLiPicasso/intro_ocaml/blob/7496a7cd6968b56eba11c84affc04a20906acfdf/OCanren_exercises/Gold_Mining/mining.ml#L121)). 

1. To reconfigure the formulae into a terser block, for this makes it easier not only for men to comprehend but also
 for computers to execute the code. This also is a prerequisite for the above "fresh distribution" step.

does not cross a whole disjunction, and choosing between tail-recursion and non-tail-recursion (neither is unconditionally better
than the other)    

## Reference

Bellman, Richard Ernest, _The Theory of Dynamic Programming_, Santa Monica, Calif.: RAND Corporation, P-550, 1954. As of July 20, 2020: https://www.rand.org/pubs/papers/P550.html





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

In general, the program is not efficient at all. All expected tasks cannot be done in a
non-trivial manner. A lot is there to be improved. A better rational number arithmetic
package is perhaps the key, which in turn requires a well understood and well-behaved
natural number arithmetic package. It is also possible that the design of
the program has some problem, but now I did not see any.


# message

Enumerate all pairs a, b up to some bound and create table to look up for a normalized representation of a/b immediately. For example, to normalize 100/2 it would be enough to look up in the table for numerator 100 and denominator 2 and immediately get 50/1 as a result.



what plan gives expectation between [1/100, 1/3] 
Break down :what plan gives expectation between [1/100, 1/3] 
into: 
(1) given a / b ==> all arithmetics expressions that evaluates to a/b 
(2) given arithmetics expression ===> mining plan 
( 2) most challanging

p[rx + p [r(1-r)x + q[sy + q[s(1-s)y]]]]

restrict the expression this the above form

