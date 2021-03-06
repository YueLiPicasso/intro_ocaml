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

(* an optional argument can be provided in two ways:
   using prefix ~lab: or ?lab: The latter way passes the 
   argument as is, while the latter automatically wraps 
   the arg with Some when passing it *)
(* providing a non-labelled non-optional argument causes all
   optional arguments before it, if there are any in the definition,
   and if they are not provided at the site of call, to be defaulted,
   i.e., their dfault values are supplied automatically. *)
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

(* type inference problems with labels *)

let f  ~x ~y = x - y;;
let h' g = g ~y:2 ~x:3;;
(* The following expression has
   a type error:
   h' f;;
   The type of f and g does not match: one is x:int -> y:int -> int
   the other is y:int-> x:int-> int. Because the compiler cannot tell
   what is the intended parameter order of g, so it assumes that the 
   way it appears is the intended order.  The problem can be fixed by
   annotation g with the intended type.  *)
let h' (g: x:int -> y:int -> int) = g ~y:2 ~x:3;;
h' f;;

let bump ?(step = 1) x = x + step;;
(* problem and solution:
 
   let bump_it bump x = bump ~step:2 x;;
   bump_it bump 1;;

   causes a type error. Because the compiler cannot tell 
   if the ~step parameter is optional, nor what is the default
   order of the two parameters, the ~step parameter is assumed 
   to be non-optional, and the default order is assumed to be 
   the order in which the arguments appear. Thus, the inferred
   type of the first parameter of bump_it is: 
   step:int -> 'a -> 'b
   which does not match the type of the bump function:
   ?step:int -> int -> int, i.e. int option -> int -> int
   Several ways exist to fix the problem.
*)

let bump_it bump x = bump ?step:(Some 2) x;; (* use optlabel *)
let bump_it bump (x : int) : int = bump ?step:(Some 2) x;;
let bump_it (bump : ?step:int -> int -> int) x = bump ~step:2 x;;
bump_it bump 1;;


let add ?(x = 0) ~x:y = ( + ) x y;;
add ~x:1 ~x:2;;
(* erronous
   add ~x:1 ?x:None;; *)
add ?x:None ~x:6;;
add 6;;

(* This is not allowed, although we can guess 
   that 2 corresponds to label x. 
let f ~x ~y = x+y in f ~y:1 2;;
*)

(* THis is neither allowed, but this time we cannot 
   guess which is which regarding the correspondence
   between the arguments (1 and 2) and the formal 
   parameters ~x and ~z.

   let f ~x ~y ~z = x,y,z in f 1 2 ~y:2;;
*)

(* There is only one case where labels can be omitted:
   - the function has a known arity, and 
   - all arguments are unlabelled, and
   - the number of arguments matches that of non-optional 
     parameters. *)
  

let f ~x ~y ~z ?(k = 0) = x * y * z + k in (f 1 2 3) ~k:4;;
let f ~x ~y ?(k = 0) ~z  = x * y * z + k in (f 1 2 3);;


(* f is expected to have type int -> int, while bump has 
   type ?step:int -> int -> int. The compiler transforms 
   bump to match the type of f by passing None as the optional 
   argument *)

let twice f (x : int) = f (f x);;

twice bump 2;;

let f ?(x = 0) ?(y = 1) z ?(a = 2) ?(b = 3) = (x + y + a + b)*z;;
twice (f ~a:0 ~b:0) 2;;

(* polymorphic variant *)

(* a value constructor could belong to several variant 
   types but it can only assume one type at a time *)

type mytype = Hello 
and yourtype  = Hello 
and histype = Bye of int
and hertype = Bye of int
and 'a theirtype = Aha of 'a
and 'a ourtype = Aha of 'a;;

  
(Hello : mytype),(Hello : yourtype),
(Bye 3 : histype),(Bye 3 : hertype),
(Aha "got it" : string theirtype),
(Aha 'c' : char ourtype);;

(* type  error
(Hello : mytype) = (Hello : yourtype);;
*)

(* a polymorphic variant constructor is 
   prefixed with a backtick *)

[`On;`Off];;
`Number 1;;
let f = function `On -> 1 | `Off -> 0 | `Number n -> n;;
List.map f [`On;`Off;`Number 5];;
List.map f [`On;`Off];;


let ls' = [`Hi; `There];;
let ls = [`Hi; `There ; `This ; `Is ; `Yue];;

(* ls has type
   [> `Hi | `Is | `There | `This | `Yue ] list
   the [> ... ] part is compatible with a variant 
   type 'b containing more tags, so the type of ls
   is compatible with 'b list.  The [> ...] part is 
   called an "open variant type".  
*)

let foo = function
    `Hi -> "hi"
  | `There -> "there"
  | `This -> "this"
  | `Is -> "is"
  | `Yue -> "yue"
  | `How -> "how"
  | `Are -> "are"
  | `You -> "you"
  | `My -> "my"
  | `Friend -> "friend"
;;

(* The function foo has type 
   [< `Are | `Friend | `Hi | `How | `Is | `My | `There | `This | `You | `Yue ]
   ->  string where the argument type of the form [< ...] is a "closed
   variant type". It enumerates all possible tags to which foo may be 
   applied. This type is compatiable with the parameter types of  both 
   ls and ls' *)

List.map foo ls;;
List.map foo ls';;

(* one more open variant type example *)

[[`This; `Is];
 [`A ; `Story];
 [`About ; `The];
 [`People ; `Of];
 [`Rome]
];;

(* exact polymorphic variant type is used in type abbreviation *)
type 'a vlist = [`Nil | `Cons of 'a * 'a vlist];;

let rec map f : 'a vlist -> 'b vlist = function
  | `Nil -> `Nil
  | `Cons(a,l) -> `Cons(f a, map f l)
;;

(* use a type constraint to prevent the type of the
   right-hand side polymorphic variant expression 
   from being inferred independently *)
let ls : float vlist = `Cons(1.2 , `Cons (2.3 , `Cons (4.4 , `Nil)));;
map Float.to_string ls;;

(* More on type checking for polymorphic variants *)
(* compare the inferred typea for h and f *)
let h = function `A -> `C | `B -> `D;;
let f = function `A -> `C | `B -> `D | x -> x;;
(* for f: since the last pattern matches any tag, the matching is open, 
   the input type of f must be compatible with [> `A | `B]. On the other
   hand, x is returned as is, so the input type and output type must be 
   the same, both being compatible with [> `A | `B]. Furthermore, the 
   return type must also be compatible with [> `C |`D ], so both input 
   and output type must be compatible with [> `A | `B | `C |`D], which 
   is captured by the type expression 
   ([> `A | `B | `C | `D] as 'a) -> 'a 
*)

f `E;;

(* f `E has type [>  `A | `B | `C | `D | `E] because `E has 
   type [> `E], and the former type is the smallest type 
   compatiable with both `E and the input type of f *)

(* conjunctive tyes *)
(* infer by hand the type of the following functions *)

let f1 = function `A x -> x = 1 | `B -> true | `C -> false;;
(* Output type is bool, x is of type int, input type should be closed, 
   i.e., [< `Tag1 | `Tag2 | ...],  containing `A of int, `B and `C *)

let f2 = function `A x -> x = "a" | `B -> true;;
(* x is of type string, output type is bool, input type is cloaed, 
   containing `A of string and `B *)

let f x = f1 x && f2 x;;
(* My asnwer is: output type is bool, and input type is an 
   intersection of the input types for f1 and f2, i.e., [< `B] *)
(* The type checker gives [< `A of int & string | `B] because both
   f1 and f2 accept the variant tag `A, despite that the argument 
   type for `A in f1 is different from that in f2. The inferred 
   value, `A of int & string, means that if we pass the tag `A to f,
   then the argument that follows `A must be of both types  int 
   and string. This is not satisfiable, so that we only only pass `B
   to f.  *)

f `B;;

(* The following expressions and type definiions cause error: 

f (`A 5);;
f (`A "hello");;

type contra = [`A of string & int];;
type contra = [`A of int & int];; 
*)

(* coercion for extact polymorphic variants *)


type 'a wlist = [`Nil
                | `Cons of 'a * 'a wlist
                | `Snoc of 'a wlist * 'a];;

let ls : int wlist = `Snoc (`Snoc (`Snoc (`Nil, 3) ,2), 1);;  
let ls' : int vlist = `Cons (1, `Cons (2, `Cons (3, `Nil)));;

let wlist_of_vlist l = (l : 'a vlist :> 'a wlist) in
wlist_of_vlist ls';;

let open_vlist l = (l : 'a vlist :> [> 'a vlist]);;
open_vlist ls';;

fun x -> (x :> [`A |`B |`C ]);;

(* Selectively coerce values through pattern matching *)
let split_cases = function
  | `Nil | `Cons _ as x -> `A x
  | `Snoc _ as x -> `B x
;;

split_cases ls;;
split_cases ls';;
split_cases (open_vlist ls');;
split_cases (wlist_of_vlist ls');;
split_cases `Nil;;

(* The or-pattern `Nil | `Cons _ (composed of variant tags) is
   wraped inside n alias-pattern. The precedence of | is higher 
   than the keyword "as"  *)

(* incremental definition of function *)

let num x = `Num x;;
let eval1 eval (`Num x) = x;;
let rec eval x = eval1 eval x;;


let plus x y = `Plus(x,y);;
let eval2 eval = function
  | `Plus(x,y) -> eval x + eval y
  | `Num _ as x -> eval1 eval x;;
let rec eval x = eval2 eval x;; (* creates a recursive type *)

eval (`Plus(`Num 4, `Plus(`Num 1, `Num 2)));;

eval (plus (num 4) (plus (num 3) (num 7)));;


(* if type-name is bound to an exact  polymorphic variant 
   type, then #type-name denotes an or-pattern for that type *)
type myvariant = [`Tag1 of int | `Tag2 of bool];;
let f = function
  | #myvariant -> "myvariant"
  | `Tag3 -> "Tag3";;

let g1 = function `Tag1 _ -> "Tag1" | `Tag2 _ -> "Tag2";;
let g = function
  | #myvariant as x -> g1 x
  | `Tag3 -> "Tag3";;


type abc = [`A | `B| `C];;
let f = function
  | `As -> "A"
  | #abc -> "other";;
let f : abc -> string = f;;

f `A;; (* f `As;; *)

(*
let f : abc -> string = function
  | `As -> "A"
  | #abc -> "other";;*)
