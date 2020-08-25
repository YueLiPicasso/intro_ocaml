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

<sequential operator> ::= if | then | else | fi | ; 

<separator> ::= :=

<empty> ::= 
```

`<empty>` is the empty string of symbols. Then comes the higher level constructs:

```
<if clause> ::= if <expression> then <statement> else <statement> fi

<assignment> ::= <variable> := <expression>

<basic statement> ::= <empty>
                    | <if clause> 
                    | <assignment> 
	      
<statement> ::= <basic statement> ;
              | <basic statement> ; <statement>
```

Since the definition of
`<if clause>` contains `<conditional>` which in turn contains `<statement>` which
in turn contains `<if clause>`, these definitions are
necessarily recursive.

Some values from `<statement>` are, for example: `;`,  `if x then ; else y := 1 fi ;` and 
`; ; x := 0 ; ;`. The semantics of `;` is, informally,  to sequence
multiple `<if clause>` and <assignment>, and single or extra `;` are ignored and nothing
else would be done.

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
`<conditional>` to
a value of `<graph>`. A generic translation algorithm is given by propagating
`{{ }}` through the BNF defintion of the labguages. Start reading from (1).


```
(1) {{ <conditional> }} ::= {{ skip }}            go to (2)
                         | {{ <statement> }}      go to (3)

(2) {{ skip }} ::= <null graph> 

(3) {{ <statement> }} ::=  {{ <if clause > ; }}                 go to (4) 
                         | {{ <assignment> ; }}                 go to (5)
	                 | {{ <if clause > ; <statement> }}     go to (6)
			 | {{ <assignment> ; <statement> }}     go to (7)	

(4) {{ <if clause> ; }} ::= {{ if <expression> then <conditional> else <conditional> fi ; }}
                        ::= mux ( <expression> ,
			         {{ <conditional> }} ,          go to (1)
				 {{ <conditional> }} )          go to (1)

(5) {{ <assignment> ; }} ::= {{ <variable> := <expression> }}
                         ::= let <variable> = <expression> in <null graph>

(6) {{ <if clause > ; <statement> }}
          ::= {{ if <expression> then <conditional> else <conditional> fi ; <statement> }}
	  ::= mux ( <expression> ,
	            {{ <conditional> <statement> }} ,
		    {{ <conditional> <statement> }} )   go to (4) or (5)
          where <stat1> ::= <stat2>
	        <stat2> ::= <stat3>
		<stat3> ::= <statement>

() {{ <assignment> ; <statement> }} ::= {{ <variable> := <expression> ; <statement> }}
                                     ::= let <variable> = <expression> in {{ <statement> }} go to (1)
                                     
```

## A Worked Example

```
x := 1 ;
y := 0 ;
if x then y
```


