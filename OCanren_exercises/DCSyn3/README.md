# Problem Description


We have a simple imperative language, whose BNF syntax is given below: 


<boolean> ::= 0 | 1

<letter> ::= u | v | w | x | y | z

<variable> ::= <letter> | <variable> <letter>

<constant> ::= <boolean> | <constant> <boolean> 

<expression> ::= <constant> | <variablle> | <variablle> [ <expr> ] | <if clause>

<if clause> ::= if <expression> then <expression> else <expression> fi

The syntax above is described with the aid of metalinguistic formulae.
Words enclosed in brackets `<>` are metalinguistic variables whose
values are sequences of symbols. The marks `::=` and `|` are metalinguistic
connectives (the latter means "or"). Any mark, which is neither a variable
nor a connective, denotes itself. Juxtaposition of
marks and/or variables signifies juxtaposition of the sequences denoted.



eg. if if 11 then x else a[10] fi then a[0] else y fi 


mux (mux (1, x,  slice(a,10)), slice(a,0), y) 

if a[i] then a[i] else x fi

mux (slice(a,i), slice(a,i),x)

simplifies to let ai = slice(a,i) in mux (ai,ai,x)


The goal :
forall s, o : eval_imp (p, s, o) /\ eval_flow (P, s, o)
what is P such that for all s and o such that eval_imp(p,s,o) 

http://minikanren.org/workshop/2020/minikanren-2020-paper9.pdf 
