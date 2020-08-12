# A Relational Translator for Digital Circuit Design




## The Imperative Language


letter ::= [a]() ... [z]()

var :: = letter { Letter }

expr ::= [0]() | [1]() | var

statement ::= var [:=]() expr
           |  [if]() expr [then]() statement [else]() statement [fi]()

program ::= { statement }
