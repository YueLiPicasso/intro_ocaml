# A Relational Translator for Digital Circuit Design



## Syntaxes of the languages concerned

The languages' syntaxes are given in BNF notation. 

1. Lowercase _italic_ words are used to denote syntactic categories.
1. **Boldface** words  denote terminal symbols.
1. A _production_ consists of a left-hand side, the symbol "::=" and a right-hand side, meaning that any occurrence of the left-hand side may be textually replaced by an instance of the right-hand side, where:
     1. A vertical bar (|) separates alternative items, any one of which can be used to replace an occurrence of the left-hand side;
     1. An ellipsis (...) enumerates alternative items when it is  verbose to list them all with vertical bars;
     1. Braces { } enclose a repeated item which can appear for zero, one or more times.


### The Imperative Language


_letter_ ::= **a** ... **z**

_var_ ::= _letter_ { _Letter_ }

_expr_ ::= **0** | **1** | _var

_statement_ ::= _var_ **:=** _expr_ | **if** _expr_ **then** _statement_ **else** _statement_ **fi**

_program_ ::= { _statement_ }
