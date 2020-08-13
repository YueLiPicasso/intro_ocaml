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
     1. Braces { } enclose a repeated item which can appear for zero, one or more times.
     


### The Imperative Language


_letter_ ::= **a** ... **z**

_var_ ::= _letter_ { _letter_ }

_expr_ ::= **0** | **1** | _var_

_statement_ ::= **if** _expr_ **then** _statement_ **else** _statement_ **fi** | _var_ **:=** _expr_ 

_program_ ::= { _statement_ }


### The flowchart language


_graph_ ::= **0** | **1** | _var_ | _grapg_ **,** _graph_
| **let** _let-binding_  **in** _graph_ | **mux(** _graph_ **,** _graph_ **,**
 _graph_ **)**

_let-binding_ ::= _var_ **=** _graph_


## Translation Design

Mapping from syntactic categories of the imperative language to syntactic
categories of the flowchart language.

The syntactic category _var_ and the terminal symbols **0** and **1** are shared
by both languages.

### _statement_ -> _graph_

**if** | _expr_ | **then** | _statement_ |**else** |_statement_ | **fi**
 ---   | ---    |   ---     |  ---        | ---     |  ---       | ---
**mux(**| _graph_| **,**    |  _graph_     | **,**   | _graph_    | **)**