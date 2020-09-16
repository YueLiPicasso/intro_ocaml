# Problem Description

## The mathematical and logic-programmatical nature of the problem

We are interested in finding equivalent functions. Given a _source function_
_f : A -> B_ we would like to find a _target function_ _g : A -> B_ such that
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
eval(i1, p?, o1) /\ eval(i2, p?, o2) /\ ... /\ eval(in, p?, on)       (*)
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
evalb(in, q, on?) /\ eval(in, p?, on?)                         (**)
```


When put into left-to-right execution `(**)` has the following
(SLD resolution style) state transition flow where _State k_ transitions to _State k+1_ in
the manner of _Transition k_.


__State 1__: The goal `(**)` above.


__Transition 1__: `evalb(i1, q, o1?)` computes a unique `o1`
that is then provided to `eval(i1, p?, o1?)`.

__State 2__:
```
                     eval(i1, p?, o1)  /\
evalb(i2, q, o2?) /\ eval(i2, p?, o2?) /\
...
evalb(in, q, on?) /\ eval(in, p?, on?)
```

__Transition 2__: `eval(i1, p?, o1)` generates a `p` such that `p(i1)=o1`
and then provides it to the rest of the goal.

__State 3__:
```
evalb(i2, q, o2?) /\ eval(i2, p, o2?) /\
...
evalb(in, q, on?) /\ eval(in, p, on?)
```

__Transition 3__: `evalb(i2, q, o2?)` computes a unique `o2` that is then
provided to `eval(i2, p, o2?)`.


__State 4__:
```
                     eval(i2, p, o2) /\
...
evalb(in, q, on?) /\ eval(in, p, on?)
```

__Transition 4__: `eval(i2, p, o2)` tests `p(i2)=o2`. If so proceed to
compute `o3` and test
`p(i3)=o3` etc., otherwise backtrack and recompute `p`.


The correspondence between `(*)` and `(**)` is more easliy seen if `(**)`
is not evaluated left-to-right but evalb-first: all `evalb` subgoals are
evaluated before any `eval` subgoal is evaluated. This way when all and
only `evalb` subgoals have been evaluated the state is exactly `(*)`. In this
sense `(**)` is also a _generate-and-test_ exercise. 

If the source function _q_ operates on an infinite domain _A_ it
might be possible to find a finite subset _B_ of _A_, called the
_complete set of samples_,
so that an equivalent function _p_ (of _q_) restricted to _B_ is also
equivalent to _q_ on _A_.


## The incarnation of the problem in the field of digital circuit synthesis
 
We have prepared above that mathematically the problem is about finding
equivalent functions and logic-programmatically it is a generate-and-test
exercise. Now we see the peculiarities that arise from the context of hardware synthesis.

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

### Simple Imperative Language:  Syntax and Semantics

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
<value> ::= <constant> | <array> | undefined

<array> ::= { <array body> }

<array body> ::= <constant> | <array body> , <constant>
```

* The value of a constant is itself.
* The value of a variable is either a constant or an array, specified by a _state_.
* The value of an expression of the form `a[i]` is defined only if the value of `a` is an array,
and the value of `i` is a constant less than the length of `a`. Otherwise it is
_undefined_.

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

### Flowchart Language: Syntax and Semantics


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
flowchart from any possible input. In example (2) there are three input
ports `a`,`x` and `y`. If we assume a state that designates that `x`
has value `011`, `a` has value `{101, 111, 000}`
 and `y` has value `111`, then we can compute the output in the following way:

`arr` is undefined since `y` is  not a valid index for `a`; `sel` is `a`
because `x` is not zero; `offset` is 0; `arr'` is `a[offset]` that is `a[0]`
 or `101`; then the output is  `mux(sel, arr', y)` that is `arr'` or `101`
 because `sel` is not zero. 



## Related work

Relational synthesis for pattern matching. [2] 

## Reference

[1] The 2014 ACM paper on Kansas Lava
https://www.csee.umbc.edu/courses/331/fall16/01/haskell/p42-gill.pdf

[2] Relational synthesis for pattern matching
http://minikanren.org/workshop/2020/minikanren-2020-paper9.pdf 
