# A Relational Translator for Digital Circuit Design

Translateing a simple imperative language program into a
flowchart language program.  

## Syntaxes of the languages concerned

The languages' syntaxes are given in BNF notation. 

1. Syntactic categories are set in _italic_ font.
1. Terminal symbols are set in **Boldface**.
1. A _production_ consists of a left-hand side, the symbol "::=" and a right-hand side, meaning that any occurrence of the left-hand side may be textually replaced by an instance of the right-hand side, where:
     1. A vertical bar ( | ) separates alternative items, any one of which can be used to replace an occurrence of the left-hand side;
     1. An ellipsis ( ... ) enumerates alternative items when it is  verbose to list them all with vertical bars;
     1. Braces { } enclose a repeated item which can appear for zero, one or more times;
     1. Square brackets [ ] enclose optional items.
     


### The Imperative Language


_letter_ ::= **a** ... **z**

_var_ ::= _letter_ { _letter_ }

_expr_ ::= **0** | **1** | _var_

_statement_ ::= **if** _expr_ **then** _statement_ **else** _statement_ **fi** | _var_ **:=** _expr_ 

_program_ ::=  _statement_  [ _program_ ]

### The flowchart language


_graph_ ::= **0** | **1** | _var_ 
| **let** _var_ **=** _graph_ **in** _graph_ | **mux(** _graph_ **,** _graph_ **,** _graph_ **)**  | **null**
 

### Semantic note

In the flowchart language **mux(** _graph_ **,** _graph_ **,** _graph_ **)**
 refers to multiplexing where the first argument is the selection signal.  

## Translation Design

 No need for imaginations on how a _graph_
might be converted into a schematic, although the context of this project is
about digital circuits. Instead, we work on a certain level of abstraction,
which is all and only about:

> Mapping from syntactic categories of the imperative language to syntactic
categories of the flowchart language. We eventually map a
_program_ to a _graph_.


The syntactic category _var_ and the terminal symbols **0** and **1** are shared
by both languages.

Table 1
_expr_ | ->    | _graph_
---   |---  | ---
**0** | ->  | **0**
**1** | ->  | **1**
_var_ | ->  | _var_


A _program_ is defined as a _statement_ followed by an optional [ _program_ ]. Translating from _program_ to _graph_ takes a case analysis on the shape of the first _statement_.


Given a _program_ of the form
_stat_ [ _prog_ ]
where the _stat_ has the form
**if** _expr_ **then** _stat1_ **else** _stat2_ **fi**
we set  _stat1_ [ _prog_ ]  as _prog1_  and set _stat2_ [ _prog_ ] as  _prog2_.
Then we convert _prog1_ and _prog2_ to _graph1_ and _graph2_ respectively. 
Assuming that _expr_ is converted to _graph0_ (by Table 1), then the whole _program_ is translated into
**mux(** _graph0_ **,** _graph1_ **,** _graph2_ **)**. This is summarized in Table 2.1.

Table 2.1
 _stat_ [ _prog_ ] | -> | _graph_   | Notes
---         | ---    |   ---  | ---
**if**     |    | **mux(**    |  
_expr_     | -> | _graph0_    | Refer to Table 1
**then**   |    | **,**       |
_stat1_    | with [ _prog_ ] -> |  _graph1_   | Recursion  
**else**   |    |**,**        |
_stat2_    | with [ _prog_ ] -> | _graph2_    | Recursion  
**fi**     |    | **)**       |      



A _program_ of the form
_stat_ [ _prog_ ], 
where the _stat_ has the form _var_ **:=** _expr_, is translated into  **let** _var_ = _graph1_ **in**  _graph2_
 where _var_ **=** _graph1_ is a straightforward translation of _var_ **:=** _expr_ and _graph2_ is a translation
 of [ _prog_ ].  If the optional [ _prog_ ]  is
missing then we supply the "null" graph
as the default image. This is summarized in Table 2.2.



Table 2.2
_stat_  [ _prog_ ] |  ->  | _graph_                         | Notes
---                | ---  |  ---                            | ---
_var_ **:=** _expr_    |  ->  |  **let** _var_ **=** _graph1_ **in**  | _expr_ -> _graph1_, see Table 1 
[ _prog_ ]         |  ->  | _graph2_                          |  Default **null**


We use Tables 1, 2.1 and 2.2 to translate any _program_ into a _graph_.




### A Worked Example

The  program:

```
x := 1
y := 0
if x then x := 0
     else if y then z := 0
               else y := 1
	  fi
fi
if x then y := 1 else z := 1 fi
```

is translated into:

```
let x = 1 in
let y = 0 in
mux( x ,
     let x = 0 in mux(x,  let y = 1 in null , let z = 1 in null) ,      
     mux( y ,
          let z = 0 in mux( x, let y = 1 in null , let z = 1 in null) ,   
	  let y = 1 in mux( x, let y = 1 in null , let z = 1 in null)))
```
The detailed steps are given in Appendix A.


## Alternative syntaxes and translation presentation 

The syntaxes so far have been presented in ways inspired by both the VHDL
standard 2019 and the OCaml manual 4.10.
The following style follows the Algol 60 report.


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


### The imperative language

A declaration of basic symbols is as follows:
```
<empty> ::= 

<delimiter> := <sequential operator> | <separator> 

<sequential operator> ::= if | then | else | fi

<separator> ::= :=
```

Then comes the higher level constructs:

```
<if clause> ::= if <expression> then <statement> else <statement> fi

<assignment> ::= <variable> := <expression> 

<statement> ::=  <if clause > | <assignment>

<program> ::=  <statement> <empty> | <statement> <program>
```

`<empty>` is the null string of symbols.  Since the definition of
`<if clause>` contains `<statement>` and vice versa, these definitions are
necessarily recursive.

### The flowchart language

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

### Translation design

We use `{{ }}` to denote a translation operation from a value of `<program>` to
a value of `<graph>`. A generic translation algorithm is given by propagating
`{{ }}` through the BNF defintion of the labguages.


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


## Appendix A

We use `{{ }}` to denote the translation function, which has the type
`program -> graph`, and show the state transition by `==>>` and show the
redex by `<<<`. 


```
{{
x := 1  <<<
y := 0
if x then x := 0
     else if y then z := 0 else y := 1 fi fi
if x then y := 1 else z := 1 fi
}}


=(Table 2.2)=>>


let x = 1 in
{{
y := 0  <<<
if x then x := 0
     else if y then z := 0 else y := 1 fi fi
if x then y := 1 else z := 1 fi
}}


=(Table 2.2)=>>


let x = 1 in
let y = 0 in
{{
if x then x := 0    <<<
     else if y then z := 0 else y := 1 fi fi
if x then y := 1 else z := 1 fi
}}


=(Table 2.1)=>>


let x = 1 in
let y = 0 in
mux( x ,
   {{ x := 0  <<<
      if x then y := 1 else z := 1 fi }} ,
   {{ if y then z := 0 else y := 1 fi
      if x then y := 1 else z := 1 fi }} )
      

=(Table 2.2)=>>


let x = 1 in
let y = 0 in
mux( x ,
     let x = 0 in
     {{ if x then y := 1 else z := 1 fi }} ,   <<<
   {{ if y then z := 0 else y := 1 fi
      if x then y := 1 else z := 1 fi }} )


=(Table 2.1)=>>


let x = 1 in
let y = 0 in
mux( x ,
     let x = 0 in
    mux( x,  {{ y := 1 }} , {{ z := 1 }} ) ,   <<<
   {{ if y then z := 0 else y := 1 fi
      if x then y := 1 else z := 1 fi }} )


=(Table 2.2)=>>


let x = 1 in
let y = 0 in
mux( x ,
     let x = 0 in mux(x,  let y = 1 in null , let z = 1 in null) ,      
   {{ if y then z := 0 else y := 1 fi        <<<
      if x then y := 1 else z := 1 fi }} )


=(Table 2.1)=>>


let x = 1 in
let y = 0 in
mux( x ,
     let x = 0 in mux(x,  let y = 1 in null , let z = 1 in null) ,      
     mux( y ,
          {{ z := 0 if x then y := 1 else z := 1 fi }} ,   <<<
	  {{ y := 1 if x then y := 1 else z := 1 fi }} ))


=(Table 2.2)=>>


let x = 1 in
let y = 0 in
mux( x ,
     let x = 0 in mux(x,  let y = 1 in null , let z = 1 in null) ,      
     mux( y ,
          let z = 0 in {{ if x then y := 1 else z := 1 fi }} ,   <<<
	  {{ y := 1 if x then y := 1 else z := 1 fi }} ))


=(Table 2.1)=>>


let x = 1 in
let y = 0 in
mux( x ,
     let x = 0 in mux(x,  let y = 1 in null , let z = 1 in null) ,      
     mux( y ,
          let z = 0 in mux( x, {{ y := 1 }} ,  {{ z := 1 }}) ,   <<<
	  {{ y := 1 if x then y := 1 else z := 1 fi }} ))



=(Table 2.2)=>>


let x = 1 in
let y = 0 in
mux( x ,
     let x = 0 in mux(x,  let y = 1 in null , let z = 1 in null) ,      
     mux( y ,
          let z = 0 in mux( x, let  y = 1 in null ,  let z = 1 in null) ,   
	  {{ y := 1 if x then y := 1 else z := 1 fi }} ))  <<<


=(Table 2.2)=>>


let x = 1 in
let y = 0 in
mux( x ,
     let x = 0 in mux(x,  let y = 1 in null , let z = 1 in null) ,      
     mux( y ,
          let z = 0 in mux( x, let  y = 1 in null ,  let z = 1 in null) ,   
	  let y = 1 {{ if x then y := 1 else z := 1 fi }} ))  <<<

=(Table 2.1)=>>


let x = 1 in
let y = 0 in
mux( x ,
     let x = 0 in mux(x,  let y = 1 in null , let z = 1 in null) ,      
     mux( y ,
          let z = 0 in mux( x, let y = 1 in null ,  let z = 1 in null) ,   
	  let y = 1 in mux( x, {{y := 1}} , {{z := 1}} )))  <<<


=(Table 2.2)=>>


let x = 1 in
let y = 0 in
mux( x ,
     let x = 0 in mux(x,  let y = 1 in null , let z = 1 in null) ,      
     mux( y ,
          let z = 0 in mux( x, let y = 1 in null , let z = 1 in null) ,   
	  let y = 1 in mux( x, let y = 1 in null , let z = 1 in null)))

```

