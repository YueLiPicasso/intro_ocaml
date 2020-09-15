# Problem Description

## The mathematical and logic-programmatical nature of the problem

We are interested in finding equivalent functions. Given a function
_f : A -> B_ we would like to find a function _g : A -> B_ such that
for all x in A, f(x) = g(x). The functions _f_ and _g_ are equal in the
sense that they have the same input-output characteristics but on the
other hand they differ in terms of the internal structures. 

More specificlly, we will use the technique of logic programminng
(aka. relational programming) to find equal-behavior programs.
Let `p` be a program such that for every valid input `i` there is
exactly one output `o`. Such a program can thus be regarded as a function.
Let `eval` be a relational interpretor for the language of `p`, then the
goal would be:
```
eval(i1, p?, o1) /\ eval(i2, p?, o2) /\ ... /\ eval(in, p?, on)
```
which reads: "What is `p` such that given `i1` it produces `o1`, 
and given `i2` it produces `o2` and  ... given `in` it produces `on`?".
This is a typical _generate-and-test_ exercise where the first atomic goal
`eval(i1, p?, o1)` is the generator and the remaining conjunction
`eval(i2, p?, o2) /\ ... /\ eval(in, p?, on)` as a whole is the tester.
The generator finds a `p` such that `p(i1) = o1` and the tester checks
that this also holds for all the I/O pairs `(i2,o2),...,(in,on)`. If the test
fails then a new `p` is generated and tested again, and so on until a `p`
is found that passes the test. A complete search strategy would find all
such `p`'s.

The notaion `?` indicates an unknown parameter, otherwise the
parameter is assumed to be knowm.

In a variation of the problem, the I/O pairs are given by another program `q`
that has the same domain and codomain as `p` but is in a different language and
thus has a dfferent (relational) interpretor `evalb`. In this case a goal
has the form:
```
evalb(i1, q, o1?) /\ eval(i1, p?, o1?) /\
evalb(i2, q, o2?) /\ eval(i2, p?, o2?) /\
...
evalb(in, q, on?) /\ eval(in, p?, on?)
```


which when put into execution acts like:

```
evalb(i1, q, o1?) /\ eval(i1, p?, o1?) /\
evalb(i2, q, o2?) /\ eval(i2, p?, o2?) /\
...
evalb(in, q, on?) /\ eval(in, p?, on?)


"evalb(i1, q, o1?) computes a unique 'o1' that is then provided
to eval(i1, p?, o1?)"
====>
                     eval(i1, p?, o1)  /\
evalb(i2, q, o2?) /\ eval(i2, p?, o2?) /\
...
evalb(in, q, on?) /\ eval(in, p?, on?)


"eval(i1, p?, o1) generates a 'p' such that p(i1)=o1 and then provide
it to the rest of the goal"
===>

evalb(i2, q, o2?) /\ eval(i2, p, o2?) /\
...
evalb(in, q, on?) /\ eval(in, p, on?)


"evalb(i2, q, o2?) computes a unique 'o2' that is then provided
to eval(i2, p, o2?)"
====>

                     eval(i2, p, o2) /\
...
evalb(in, q, on?) /\ eval(in, p, on?)


" eval(i2, p, o2) tests p(i2)=o2. If so proceed to compute 'o3' and test
p(i3)=o3, otherwise backtrack and recompute "
```


Certain programs such as a pattern matching specification, a switch
 language program, a sequential description of  



## The incarnation of the problem in the field of dgital crcuit synthesis

We work on the problem of finding equal function 


The problem concerns translation between two languages using
relational programming techniques, in the context of hardware synthesis.. 

There is a simple imperative language and a flowchart language, and they
share the following syntactic categories:

```
<boolean> ::= 0 | 1

<constant> ::= <boolean> | <constant> <boolean> 

<letter> ::= a | b | c | d | e | f | g
           | h | i | j | k | l | m | n
	   | o | p | q | r | s | t | u
	   | v | w | x | y | z

<variable> ::= <letter> | <variable> <letter>
```

The syntax above is described with the aid of metalinguistic formulae.
Words enclosed in brackets `<>` are metalinguistic variables whose
values are sequences of symbols. The marks `::=` and `|` are metalinguistic
connectives (the latter means "or"). Any mark, which is neither a variable
nor a connective, denotes itself. Juxtaposition of
marks and/or variables signifies juxtaposition of the sequences denoted.

## Simple Imperative Language:  Syntax and Semantics

Furthermore, the simple imperative language has the following unique
syntactic categories. 

```
<expression> ::= <constant>
               | <variablle>
	       | <variablle> [ <expression> ]
	       | <if clause>

<if clause> ::= if <expression> then <expression> else <expression> fi
```
We now give the informal semantics of the imperative language in terms of
evaluating an expression to obtain its _value_.

```
<value> ::= <constant> | <array> 

<array> ::= { <array body> }

<array body> ::= <constant> | <array body> , <constant>
```

* The value of a constant is itself.
* The value of a variable is either a constant or an array, specified by a _state_.
* The value of an expression of the form `a[i]` is defined only if the value of `a` is an array,
and the value of `i` is a constant less than the length of `a`.

As usual, array indices start from 0, and grow like 0, 1, 10, 11,
100, 101, 110, etc. If leading 0's appear, they are ignored.
For example `a[1]`, `a[01]`, `a[001]` etc all refer to the second cell of the array `a`.  

The semantics of if-clauses is given by an example. Consider:

```
EXAMPLE (1).

if if x then a else a[y] fi
 then
   a[if 0 then 1 else 0 fi]
 else
   y
fi 
```
There are three variables `x`, `a` and `y`. One 
state designates that `x` has value `011`, `a` has value `{101, 111, 000}`
 and `y` has value `111`. Then
`if x then a else a[y] fi` evaluates to `{101, 111, 000}` and the top-level clause
evaluates to the value of `a[if 0 then 1 else 0 fi]` which is `101`.

## Flowchart Language: Syntax and Semantics


The flowchart language, modelled after the Lava HDL [1], has the following
unique syntactic categories:

```
<fan-out> ::= let <variable> = <signal> in <signal>

<multiplexing> ::= mux ( <signal> , <signal> , <signal> )

<slicing> ::= slice ( <signal> , <signal> )

<signal> ::= <constant> | <variable> | <fan-out> | <multiplexing> | <slicing>
```

where the `<signal>` category represents an input, output or internal wire in a
circuit.

The program below encodes [this flowchart](https://github.com/YueLiPicasso/intro_ocaml/issues/1#issue-696667586).

```
EXAMPLE (2).

let arr = slice (a , y) in
let sel = mux (x , a , arr) in
let offet = mux (0 , 1 , 0) in
let arr' = slice (a, offset) in
mux (sel , arr', y)
```
The semantics of the langauge, informally, is to derive the output of a
flowchart from any possible input. 


## Reference

[1] The 2014 ACM paper on Kansas Lava
https://www.csee.umbc.edu/courses/331/fall16/01/haskell/p42-gill.pdf