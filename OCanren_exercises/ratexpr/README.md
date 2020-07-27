# Arithmetical Expressions of (Positive) Rational Numbers


The motivation of this OCanren project is to improve the performance of programs that maniplate rational numbers.

In particlular, it has been observed that the program for the [stochastic gold mining puzzle](../Gold_Mining) has
 performance that is below expectations. Essentially, that program computes values for arithmetical expressions
 involving (positive) rational numbers. The idea is to, instead, let the program produce an arithemtical expression
 which could be evaluated independently. We hope to rewrite the gold mining puzzle's program based on this project,
  so that instead of returning a rational number, it now returns an arithmetical expression that supposedly evaluates
  to that rational number.


## Reference

Euclidean Algorithm :

https://crypto.stanford.edu/pbc/notes/numbertheory/euclid.html

https://mathworld.wolfram.com/EuclideanAlgorithm.html