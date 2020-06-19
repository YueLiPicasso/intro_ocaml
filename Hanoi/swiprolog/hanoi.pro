/* type M-x prolog-mode to switch to Emacs Prolog mode */
/* change directory to the source directory then run
   'swipl hanoi.pl' */
/* From swipl, run the goal ?- make. to reload modified source. */

/* The three rods are denoted by a, b and c.
   The predicate freeRod(X,Y,Z) says that rod Z is the 
   auxiliary rod involved in the task of moving disks
   from rod X to rod Y */
    
freeRod(a,b,c).
freeRod(b,a,c).
freeRod(a,c,b).
freeRod(c,a,b).
freeRod(b,c,a).
freeRod(c,b,a).

/* append3 is a special case of appending three lists:
   an arbitrary list L1, a singleton list [M], and an 
    list L2  */

append3([], [X], L, [X|L]).
append3([H|T],[X], L, [H|L2]) :- append3(T,[X],L,L2).

/* Peano numbers are used to count the number of disks */

/* For computation in the default direction */

hanoi(s(o), A, B,[(A,B)]).
hanoi(s(s(N)), A, B, M) :- freeRod(A,B,C),
			   hanoi(s(N), A, C, M1),
			   hanoi(s(o), A, B, M2),
			   hanoi(s(N), C, B, M3),
			   append3(M1,M2,M3,M).

/* Example query:
?- hanoi(s(s(s(o))), a, c, M).  */


/* For reverse computation */

/* true if two lists are both non-empty and of the same length */
same_len([_],[_]).
same_len([_,A|T],[_,B|T2]) :- same_len([A|T],[B|T2]).
   
hanoir(s(o), A, B,[(A,B)]).
hanoir(s(s(N)), A, B, M) :- append3(M1,M2,M3,M),
			    same_len(M1,M3),
			    hanoir(s(o), A, B, M2),
			    freeRod(A,B,C),
			    hanoir(s(N), A, C, M1),
			    hanoir(s(N), C, B, M3).
			   
/* Example query:
M = [(a, c),  (a, b),  (c, b),  (a, c),  (b, a),  (b, c),  (a, c)],
hanoir(N,A,B,M). */
