# Problem Description


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


Furthermore, The simple imperative language has the following unique
syntactic categories. 

```
<expression> ::= <constant>
               | <variablle>
	       | <variablle> [ <expression> ]
	       | <if clause>

<if clause> ::= if <expression> then <expression> else <expression> fi
```

The `<variable> [ <expression> ]` construct denotes arrays.
Regarding  arrays and variables, for now we do not
concern ourselves with value assigment to them,
but We allow reading booleans off them, i.e., a
variable equals either 0 or 1, and an array is just a fixed-length
list of 0's and 1's.


We also have a flowchart language:

```
```
<fan-out> ::= let <variable> = <graph> in <graph>

<multiplexing> ::= mux ( <graph> , <graph> , <graph> )

<graph> ::= <expression> | <fan-out> | <multiplexing> | <null graph>
```
```

eg. if if 11 then x else a[10] fi then a[0] else y fi 


mux (mux (1, x,  slice(a,10)), slice(a,0), y) 

if a[i] then a[i] else x fi

mux (slice(a,i), slice(a,i),x)

simplifies to let ai = slice(a,i) in mux (ai,ai,x)


The goal :
forall s, o : eval_imp (p, s, o) /\ eval_flow (P, s, o)
what is P such that for all s and o such that eval_imp(p,s,o) 

http://minikanren.org/workshop/2020/minikanren-2020-paper9.pdf 
