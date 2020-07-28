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

## Remarks


Regarding quick simplification of rational numbers,
Dmitri Boulytchev suggested to enumerate all pairs (a, b) up to some bound and
to create a table to
look up for a normalized representation of a/b immediately.
For example, to normalize 100/2 it would be enough to look up in the
table for numerator 100 and denominator 2 and immediately get 50/1 as a result.