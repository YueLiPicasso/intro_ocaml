(* define an object class *)

class point =
  object
    val mutable x = 0
    method get_x = x
    method move d = x <- x + d
  end;;


(* Create a new object *)

let p = new point;;

(* call the object's methods *)

p#get_x;;

p#move 3;;

(* Evaluation of the class body takes place at
   each object-creation time *)

let x0 = ref 0;;

class point =
  object
    val mutable x = incr x0; !x0
    method get_x = x
    method move d = x <- x + d
  end;;


(* run this several times: we get a fresh x every time  *)

new point#get_x;;


(* abstract a class over initial values *)

(* syntax 1 of 2 *)
class point = fun x_init ->
  object
    val mutable x = x_init
    method get_x = x
    method move d = x <- x + d
  end;;

(*syntax 2 of 2 *)
class point x_init =
  object
    val mutable x = x_init
    method get_x = x
    method move d = x <- x + d
  end;;

  
new point;;

(* The initial value can be referenced by methods *)

let p = new point 7;;

class point x_init =
  object
    val mutable x = x_init
    method get_x = x
    method get_offset = x - x_init
    method move d = x <- x + d
  end;;

let p = new point 7;;

(* alternately run the two commands below
   for several times, we see that the initial 
   value of p is remembered. If we move 
   p back by proiding a suitable argument 
   to the "move" method, ww can see that 
   the offset changes from a non-zero value 
   back  to zero *)

p#get_offset;;

p#move (-6);;


class adjusted_point x_init =
  let origin = (x_init / 10) * 10 in
  object
    val mutable x = origin
    method get_x = x
    method get_offset = x - origin
    method move d = x <- x + d
  end;;

let ap = new adjusted_point 7;;

ap#get_x;;

ap#move 10;;

ap#get_offset;;

(* an equivalent definition of the adjusted_point class *)

class adjusted_point x_init = point ((x_init / 10) * 10);;

(* With respect to  the above shorter definition for the 
   class "adjusted_point":  What is the relationship between
   the class "point" and the class "adjusted_point" ? *)

let ap = new adjusted_point 7;; ap;; (* ap : adjusted_point = <obj>*)
let p = new point 0;; p;;  (* p : point = <obj>*)

ap#get_x;; (* 0 *)
p#get_x;; (* 0 *)
ap = p;; (* false *)

(* 
This didn't work: 

let ap = adjusted_point 7;;

adjusted_point was seen as an unbound value
*)
