# Problem Description

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


The flowchart language (modelled after the Lava HDL) has the following
unique syntactic categories:

```
<fan-out> ::= let <variable> = <signal> in <signal>

<multiplexing> ::= mux ( <signal> , <signal> , <signal> )

<slicing> ::= slice ( <signal> , <signal> )

<signal> ::= <constant> | <variable> | <fan-out> | <multiplexing> | <slicing>
```





