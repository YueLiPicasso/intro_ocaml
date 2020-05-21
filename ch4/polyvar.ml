let f ~x ~y = x - y;;
let x = 3 and y = 2 in f ~x ~y;;
(* prefixing an argument with a tilde gives that 
   argument a name, which is the same as the argument itself *)

(* we can make an argument  distinct from its label *)
let f ~x:x1 ~y:y1 = x1 - y1;;
f ~x:3 ~y:2;;
f ~y:1 ~x:8;; 

ListLabels.fold_left [1;2;3] ~init:0 ~f:( + );;
ListLabels.fold_left ~init:0;;

(* arguments bearing the same label cannot commute among themselves,
   but can still commute with other arguments *)

let hline ~x:x1 ~x:x2 ~y = (x1, x2, y);;
hline ~x:3 ~y:2 ~x:5;;

(* labels can be omitted for total applications *)
hline 1 2 3;;
ListLabels.map succ [1;2;3];;

(* a function whose result type is a type variable 
   wiil nenver be considered as being totally applied, so that
   the following expr causes an error:

   ListLabels.fold_left ( + ) 0 [1;2;3];; *)

(* to call a labeled function that is never regarded as 
   being totally applied, we must provide labels *)
ListLabels.fold_left ~f:( + ) ~init:0 [1;2;3];;
ListLabels.fold_left [1;2;3];;

(* when a function f is passed as an argument to a higher-order function
   h, labels in the type of f and labels in the type of h must match *)

let f ~y ~x = x + y and h g = g  ~y:3 ~x:2 in
h f, h (fun ~y:_ ~x -> x+1);;

(* In these cases the labels do not match, causing errors
let f ~x ~y = x + y and h g = g  ~y:3 ~x:2 in h f;;
let h g = g  ~y:3 ~x:2 in h ( + );;
*)

(* ?lab is a label for an optional argument *)
let bump ?(step = 1) x = x + step;;
bump 2;;
bump ~step:3 2;;
bump ?step:(Some 3) 2;;

let test ?(x = 0) ?(y = 0) () ?(z = 0) () = (x, y, z);;

test () ();;
test () ~z:3 ();;
test ~x:1 ~y:2 () ~z:3 ();;
(* Applied simultaneously, optional arguments can commute 
   with non-optional ones *)
test ~y:2 ~x:3 () ();;
test () () ~z:1 ~y:2 ~x:3;;

let test2 () ?(x = 1) ?(y = 1) ?(z = 1) = (x , y , z);;
test2();;
test2 () ~x:3;;
test2 ~x:3 () ~y:4 ~z:5;;

let test3 ~(x : int) ?(y = 0) ?(z = 0) = (x,y,z);;
test3 5;;
test3~x:8 ~y:6;;
test3 ~z:7 ~y:8 ~x:3;;

(* If a non-labeled argument is passed, and its corresponding parameter
   is preceded by one or several optional parameter,then these parameters
   are defaulted *)
let test4 ?(x = 0) ~y = (x,y);;
test4 6;;
test4 ~y:6;;

(* an optional parameter is essentially a parameter of the type 'a option *)
let bump ?step x =
  match step with
  | None -> x * 2
  | Some y -> x + y
;;

bump 4;;
bump ~step:3 4;;
bump ?step:(Some 4) 7;;


(* optional aruments can be relayed from one 
   function call to another *)

let test5 ?x ?y () = test ?x ?y () ();;
test5 ();;
test5 ~x:1 ();;
test5 ~x:5 ?y:None ();;

(* the main concern with optional arguments, is that when 
   they are absent, should the expression be considered as 
   partial application, with these optional arguments to be 
   provided later on, or, should these optional arguments be
   defaulted, i.e., their default values being passed to the 
   calling function *)
