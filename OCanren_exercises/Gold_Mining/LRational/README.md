# LRational

An OCanren library for positive rational numbers. A rational is represented
 as a pair of natural numbers. 

## Function

The library provides means for

* injection/projection into/out of the logic domains,
* converting integer pairs into (positive) rationals,
* relational arithmetic: multiplication, division and  addition (the addition relation can also be used for subtraction).  

## Limitation

* Non-zero denominator is _not_ enforced.
* Simplification based on greatest common divisor is _not_ supported.

## Usage

The library itself can be compiled into OCaml library files (`.cma` and`.cmxa`) using the Makefile.
This also creates `.cmi` and `.cmx` (or `.cmo`) files that can be referenced by user programs.
See the
[OCaml reference manual](http://caml.inria.fr/pub/docs/manual-ocaml/index.html) chapters
on `ocamlc` and `ocamlopt` and `ocamldep`.

## Discussion


Regarding quick simplification of rational numbers,
Dmitri Boulytchev suggested to enumerate all pairs (a, b) up to some bound and
to create a table to
look up for a normalized representation of a/b immediately.
For example, to normalize 100/2 it would be enough to look up in the
table for numerator 100 and denominator 2 and immediately get 50/1 as a result.

The above taken into account, there are several possible ways as I could see in which the simplification relation may be implemented:

1. pure-relational
    1. table
    1. naive math
1. pseudo-relational
    1. table
    1. functions in wrapper
1. preprocessor
1. miscellaneous

### The `pure-relational/table` approach

It shall be easy to generate a long list like this:

```
[... ; ((2,2),(1,1)) ; ((2,3),(2,3)) ; ((2,4),(1,2)) ; ... ]
```

which enumerates all pairs on `N x N'` for some `N = {0, 1, 2,..., n}` and `N' = {1, 2, 3,..., n}`.

However, to convert this table into a relation like

```ocaml
let simplify a b c d =
conde [... ; (?& [a === 2 ; b === 2 ; c === 1; d === 1] ) ;
             (?& [a === 2 ; b === 3 ; c === 2; d === 3] ) ;
             (?& [a === 2 ; b === 4 ; c === 1; d === 2] ) ; ... ] ;;
```

is not easy when done by hand, suppose `n = max_int`.  

I may define a printer to do this:

```ocaml
let rec make_str_list = function
     [] -> []
  |  ((a,b),(c,d)) :: tl ->
     (sprintf "(?& [a === %d ; b === %d ; c === %d; d === %d] ) ;  \n" a b c d)  :: make_str_list tl 
and build_table = (* function body *)
in
printf "let simplify a b c d = \n conde [ \n" ;
List.iter print_string @@ make_str_list @@ build_table args ;
printf "\n ] ;; \n" ;;
```

### The `pure-relational/naive math` approach

Finding the greatest common divisor for two numbers m and n (suppose m > n) could
   be as simple as enumerating all k from {1,2,.., n} and find the largest that divides both m and n.
   For this we shall define a relation that checks if one natural number divides another, something like:
   
```ocaml
(** a is dividable by b *)
let rec divido a b =
   let open LNat in
   conde [(?& [a === zero ; b =/= zero]); 
          (?& [a =/= zero ; a === b   ]);
          (?& [b =/= zero ; a > b ; Fresh.one (fun c -> minuso a b c &&& divido c b)]);
	 ];;
```


### The `pseudo-relational` approaches

As we can wrap a query within a function, we might also wrap a function within a goal.
This wrapped function can either compute by itself or refer to some data structure like a hash table.
I shall also check the OCaml standard libraries to see what might help.  


### Miscellaneous

Defining a special purpose camlp5 preprocessor? It shall convert

```
[... ; ((2,2),(1,1)) ; ((2,3),(2,3)) ; ((2,4),(1,2)) ; ... ]
```
 directly into

```ocaml
let simplify a b c d =
conde [... ; (?& [a === 2 ; b === 2 ; c === 1; d === 1] ) ;
             (?& [a === 2 ; b === 3 ; c === 2; d === 3] ) ;
             (?& [a === 2 ; b === 4 ; c === 1; d === 2] ) ; ... ] ;;
```









