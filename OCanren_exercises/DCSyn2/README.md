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
<empty> ::= 

<delimiter> := <sequential operator> | <separator> 

<sequential operator> ::= if | then | else | fi | ;

<separator> ::= :=
```

`<empty>` is the null string of symbols. Then comes the higher level constructs:

```
<if clause> ::= if <expression> then <statement> else <statement> fi

<assignment> ::= <variable> := <expression> 

<statement> ::=  <if clause >
               | <assignment>
	       | <statement> ; <statement>
	       | <statement> ; <empty>
```

A statement is an if-clause, an assignment or a sequence
of statements. Since the definition of
`<if clause>` contains `<statement>` and vice versa, these definitions are
necessarily recursive.  

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

We aim to map from <statement> to <graph>.

We use `{{ }}` to denote a translation operation from a value of
`<statement>` to
a value of `<graph>`. A generic translation algorithm is given by propagating
`{{ }}` through the BNF defintion of the labguages. Start reading from (1).


```
(1)  {{ <program> }} ::= {{ <statement> <empty> }}       go to (2)
                       | {{ <statement> <program> }}     go to (3)

(2)  {{ <statement> <empty> }} ::= {{ <if clause > <empty> }}     go to (4)
                                 | {{ <assignment> <empty> }}     go to (6)
			    
(3)  {{ <statement> <program> }} ::= {{ <if clause > <program> }}   go to (5)	  
   	                           | {{ <assignment> <program> }}   go to (7)
		  
(4)  {{ <if clause > <empty> }}
     ::= {{ if <expression> then <statement> else <statement> fi <empty> }}     go to (8)

(5)  {{ <if clause > <program> }}
     ::=  {{ if <expression> then <statement> else <statement> fi <program> }}  go to (9)

(6)  {{ <assignment> <empty> }} ::= {{ <variable> := <expression> <empty> }}      go to (10)
		  
(7)  {{ <assignment> <program> }} ::= {{ <variable> := <expression> <program> }}  go to (11)

(8)  {{ if <expression> then <statement> else <statement> fi <empty> }}
     ::= mux (<expression>, {{ <statement> <empty> }}, {{ <statement> <empty> }})      go to (2)

(9)  {{ if <expression> then <statement> else <statement> fi <program> }}
     ::= mux (<expression>, {{ <statement> <program> }}, {{ <statement> <program> }})  go to (3)
   
(10) {{ <variable> := <expression> <empty> }}
     ::= let <variable> = <expression> in <null graph>

(11) {{ <variable> := <expression> <program> }}
     ::= let <variable> = <expression> in {{ <program> }}   go to (1)
```




