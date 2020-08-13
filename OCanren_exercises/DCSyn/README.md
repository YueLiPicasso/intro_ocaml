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


_graph_ ::= **0** | **1** | _var_ | _grapg_ **,** _graph_
| **let** _let-binding_  **in** _graph_ | **mux(** _graph_ **,** _graph_ **,**
 _graph_ **)**  | **null**

_let-binding_ ::= _var_ **=** _graph_


## Translation Design

Mapping from syntactic categories of the imperative language to syntactic
categories of the flowchart language . At the highest level we map a
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
 _statement_ | -> | _let-binging_  | Notes
---         | ---    |   ---  | ---
_var_       | ->     | _var_  |
**:+**      |        | **=**  |
_expr_      |  ->    | _graph_ | Reference to Table 1


Table 2.1 and 2.2 combined, we complete the translation of _statement_ into
the syntactic categories of the flowchart language. Finally we translate
from _program_ to _graph_, in the way of Table 3.1 and 3.2. 

Table 3.1
_program_   |  ->  | _graph_  
---         | ---  |  ---     
           |      |   **(**   
_statement_ | ->   | _graph_  
           |       |   **,**  
[ _program_ ] |   ->  | _graph_  (Default **null**)
           |       |   **)**  


Table 3.2
_program_   |  ->  | _graph_
---         | ---  |  ---
           |      |   **let**
_statement_ | ->   | _let-binding_
           |       |   ** in **
[ _program_ ] |   ->  | _graph_ (Default **null**)
           |       |   **)**




