# Arithmetic Expressions of (Positive) Rational Numbers

Work in progress.

## Goal

Relational interpretation of arithmetic expressions involving positive rational numbers.
It shall be able:

- to evaluate a given expression to the normal form,
- to generate expressions that evaluate to a given normal form, and
- to generate normal form expressions, i.e., rational numbers whose numerator and denomnator are co-prime.

The program shall be as relational as
possible, supporting fruitful execution in different directions without optimization
for any certain direction. 

## History

The motivation of this OCanren project originally was to improve the performance of
programs that maniplate rational numbers. In particlular, it was observed that the
program for the [stochastic gold mining puzzle](../Gold_Mining) could barely
compute useful answers. 
 Essentially, that program evaluates arithmetic expressions
 on involving (positive) rational numbers. My idea was to, instead, let the program
 produce an arithemtical expression which could be evaluated independently.
 

Later on we found that the performance problem of the gold mining puzzle was not with
rational numbers but with the order of conjuncts in the relation definition. A proper treatment of
this aspect improved the program to a satisfactory level. Then we no longer need an
expression evaluation part for the gold mining puzzle. 


## Reference

Euclidean Algorithm :

https://crypto.stanford.edu/pbc/notes/numbertheory/euclid.html

https://mathworld.wolfram.com/EuclideanAlgorithm.html