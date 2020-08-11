# Arithmetic Expressions of (Positive) Rational Numbers

## Goal

Relational interpretation of arithmetic expressions involving positive rational numbers.
It shall be able:

- to evaluate a given expression to the normal form,
- to generate expressions that evaluate to a given normal form, and
- to generate normal form expressions, i.e., rational numbers whose numerator and denomnator are co-prime.

The program shall be as relational as
possible, supporting fruitful execution in different directions without optimization
for any certain direction.

## Result

For non-negative rational numbers,
I came up with
- a modest relational adder and
- a satifactory relational simplifier, that is  based on successful implementations of 
    - a relational division algorithm for natural numbers and
    - a relational Euclidean algorithm.

The relational simplifier can:

- simplify a rational number to the normal form
- scale up a normal form rational number 
- find rational numbers whose numerator and denominator are co-prime.

See [here](history/Solution_I) for more technical discussions about the simplifier.

The relational adder can:

- add two rational numbers
- subtract one rational number from another (but the search efficiency depends on input values)
- split a rational number `r` into two rational numbers  that sum up to `r`.

There is also a relational multiplier for rationals that does not work well backward,
and a relational evaluator for (non-neg.) rational number arithmetic expressions
whose  performance is hindered by the relational adder.

## Discussion

Defining the type for arithmetic expresssions in OCanren is an
interestng task. Insights on OCanren's type system and syntactic transformation was also gained
when writing the `Inj` module for injection primitives. Commutativity of arithmetic relations
is a consideration. For example if `GCD(a,b,c)` holds, which  means that `c` is the GCD
of `a` and `b`, then `GCD(b,a,c)` also holds. I found it conceptually helpful to distinguish
a commutative implementation of `GCD` from a non-commutative implementation, the latter of which
can be used to define the former. Alternative solutions exist for almost every relation name
defined in this project: they are all written down and tested and only the pleasant ones are
kept in the final source code and the rest are left in the commit records. 

## History

The motivation of this OCanren project originally was to improve the performance of
programs that maniplate rational numbers. In particlular, it was observed that the
program for the [stochastic gold mining puzzle](../Gold_Mining) could barely
compute useful answers. 
 Essentially, that program evaluates arithmetic expressions
 on  (positive) rational numbers. My idea was to, instead, let the program
 produce an arithemtical expression which could be evaluated independently.
 

Later on we found that the performance problem of the gold mining puzzle was not with
rational numbers but with the order of conjuncts in the relation definition. A proper treatment of
this aspect improved the program to a satisfactory level. Then we no longer need an
expression evaluation part for the gold mining puzzle. See also the [history](history) folder.



## Reference

Euclidean Algorithm for GCD and LCM:

https://mathworld.wolfram.com/EuclideanAlgorithm.html

https://mathworld.wolfram.com/LeastCommonMultiple.html