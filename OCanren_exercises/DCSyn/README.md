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


_graph_ ::= **0** | **1** | _var_ | _graph_ **,** _graph_
| **let** _let-binding_  **in** _graph_ | **mux(** _graph_ **,** _graph_ **,** _graph_ **)**  | **null**

_let-binding_ ::= _var_ **=** _graph_

### Semantic note

In the flowchart language _graph_ **,** _graph_ refers to  parallel composition. **mux(** _graph_ **,** _graph_ **,** _graph_ **)**
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


The "if...then...else" part of _statement_ is
translated into multiplexing (mux) of _graph_.

Table 2.1
 _statement_ | -> | _graph_   | Notes
---         | ---    |   ---  | ---
**if**     |    | **mux(**    |  
_expr_     | -> | _graph_     | Reference to Table 1
**then**   |    | **,**       |
_statement_| -> | _graph_     | Recursion of Table 2.1
**else**   |    |**,**        |
_statement_| -> | _graph_     | Recursion of Table 2.1
**fi**     |    | **)**       |      


The assignment ":=" part of _statement_ is translated into
_let-binding_.


Table 2.2
 _statement_ | -> | _let-binding_  | Notes
---         | ---    |   ---  | ---
_var_       | ->     | _var_  |
**:=**      |        | **=**  |
_expr_      |  ->    | _graph_ | Reference to Table 1


Table 2.1 and 2.2 combined, we complete the translation of _statement_ into
the syntactic categories of the flowchart language. Finally we translate
from _program_ to _graph_, in the way of Rewriting Rule 3.1 and Table 3.2. 

Rule 3.1

>Given a _program_ of the form

>_statement_ [ _prog_ ]

>where _statement_ has the form

>**if** _expr_ **then** _stat1_ **else** _stat2_ **fi**

>we set _prog1_ to _stat1_ [ _prog_ ] and _prog2_ to _stat2_ [ _prog_ ],

>and then convert _prog1_ and _prog2_ to _graph1_ and _graph2_ respectively. Further

>assume that _expr_ is converted to _graph0_. Then the whole _program is translated into

>**mux(** _graph0_ **,** _graph1_ **,** _graph2_ **)**.


Table 3.2
_program_   |  ->  | _graph_ | Default
---         | ---  |  ---    | ---
_statement_ | ->   |  **let** _let-binding_ **in** | 
[ _program_ ] |   ->  | _graph_  | **null**


A _program_ is defined as a _statement_ followed by an optional [ _program_ ]. When
the _statement_ is mapped to "mux", then with the _graph_ of the [ _program_ ] it forms
a parallel composition, otherwise the _statement_ is mapped to a _let-binding_ and with the
_graph_ of the [ _program_ ] it forms a "let...in..." structure.

If the optional [ _program_ ]  is
missing, i.e., if the _statement_ is at the end of the _program_ then we supply the "null" graph
as the default image of the missing optional  [ _program_ ]. Otherwie we recursively translate
[ _program_ ].


### A Worked Example

We translate this program:

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
Using the rue, we have : oops it breaks down !
```
let x = 1 in
let y = 0 in
mux( x , let x = 0 in, 
     mux( y, let z = 0 in, let y = 1 in )),
mux( x, let y = 1 in, let z = 1 in), null

```

I should have something like this 

```
let x = 1 in
let y = 0 in
mux( x , let x = 0 in
         mux( x, let y = 1 in null, let z = 1 in null), null, 
     mux( y, let z = 0 in
             mux( x, let y = 1 in null, let z = 1 in null), null,
	     let y = 1 in
	     mux( x, let y = 1 in null, let z = 1 in null), null)),

```