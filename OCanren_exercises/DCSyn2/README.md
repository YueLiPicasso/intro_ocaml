# A Relational Translator for Digital Circuit Design

Translateing from a simple imperative language program to a
flowchart language program, and backward.


## Syntaxes 

The syntaxes will be described with the aid of metalinguistic formulae.
Words enclosed in brackets `<>` are metalinguistic variables whose
values are sequences of symbols. The marks `::=` and `|` are metalinguistic
connectives (the latter means "or"). Any mark, which is neither a variable
nor a connective, denotes itself. Juxtaposition of
marks and/or variables signifies juxtaposition of the sequences denoted.


### Shared syntax

```
<boolean> ::= 0 | 1

<letter> ::= u | v | w | x | y | z

<variable> ::= <letter> | <variable> <letter>

<expression> ::= <boolean> | <variablle>
```


### The Imperative Language

A declaration of basic symbols is as follows:

```
<delimiter> := <sequential operator> | <separator> 

<sequential operator> ::= if | then | else | fi | ; | skip

<separator> ::= :=
```

Operator `;` is right associative. Then comes the higher level constructs:

```
<if clause> ::= if <expression> then <statement> else <statement> fi

<assignment> ::= <variable> := <expression>

<no action> ::= skip

<basic statement> ::=  <if clause> | <assignment> | <no action>
	      
<statement> ::= <basic statement> 
              | <basic statement> ; <statement>
```

Since the definition of
`<if clause>` contains `<statement>` which in turn contains `<basic statement>` which
in turn contains `<if clause>`, these definitions are
necessarily recursive.

Moreover,  we have the following proposition

```
<statement> ::= <statement> ; <statement>
```

which is provable by induction on the length of the left operand of `;` and which says that the
syntactic category `<statement>` is closed under the operator `;`.

Some values from `<statement>` are, for example: `skip`,  `if x then skip else x := 1 ; y := 1 fi` and 
`skip; skip; x := 0; skip; skip`. The semantics of `;` is, informally,  to sequence
multiple `<basic statement>`.  

### The Flowchart Language

A declaration of basic symbols is as follows:
```
<fan-out operator> ::= let | in

<separator> ::= , | = 

<bracket> ::= ( | )

<function> ::= mux

<null graph> ::= null
```

Then comes the higher level constructs:

```
<fan-out> ::= let <variable> = <graph> in <graph>

<multiplexing> ::= mux ( <graph> , <graph> , <graph> )

<graph> ::= <expression> | <fan-out> | <multiplexing> | <null graph>
```


## Translation

We aim to map from `<statement>` to `<graph>`.

We use `{{ }}` to denote a translation operation from a value of
`<statement>` to a value of `<graph>`. A generic translation algorithm is given by propagating
`{{ }}` through the BNF defintion of the labguages. Start reading from (1).


```
(1) {{ <statement> }} ::= {{ <basic statement> }}                  go to (2) 
                        | {{ <basic statement> ; <statement> }}    go to (6)

(2) {{ <basic statement> }} ::= {{ <if clause > }}    go to (3) 
                              | {{ <assignment> }}    go to (4)
	                      | {{ <no action> }}     go to (5)
			

(3) {{ <if clause> }} ::= {{ if <expression> then <statement> else <statement> fi }}
                      ::= mux ( <expression> ,
              	                {{ <statement> }} ,          go to (1)
		                {{ <statement> }} )          go to (1)

(4) {{ <assignment> }} ::= {{ <variable> := <expression> }}
                       ::= let <variable> = <expression> in <null graph>

(5) {{ <no action> }} ::= {{ skip }} ::= <null graph>

(6) {{ <basic statement> ; <statement> }} ::= {{ <if clause> ; <statement> }}    go to (7)
                                            | {{ <assignment> ; <statement> }}   go to ()
					    | {{ <no action> ; <statement> }}    go to ()

(7) {{ <if clause> ; <statement> }} ::= {{ if <expression> then <stat1> else <stat2> fi ; <statement> }}
                                        where <stat1> ::= <stat2> ::= <statement>
	                            ::= mux ( <expression> ,
	                                      {{ <stat1> ; <statement> }} ,   go to (1)
		                              {{ <stat2> ; <statement> }} )   go to (1)
					      
                                    

() {{ <assignment> ; <statement> }} ::= {{ <variable> := <expression> ; <statement> }}
                                     ::= let <variable> = <expression> in {{ <statement> }} go to (1)
                                     
```

## A Worked Example

```
x := 1 ;
y := 0 ;
if x then y
```


