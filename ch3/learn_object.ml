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


(* The code for adjustment, i.e., ((x_init / 10) * 10), 
   is part of the definition of the class "adjusted_point",
   therefore, it will be inherited, says the reference manual. *)


class adjusted_point' x_init = point (((x_init / 10) + 1) * 10);;


let ap = new adjusted_point 18;;

let ap' = new adjusted_point' 18;;

ap # get_x;;

ap' # get_x;;

ap # move 5;;

ap' # move 5;;

(* ap and ap' share the same methods as from the class "point" but 
   they belong to distinct classes both of which are in turn different
   from the class "point". Adjusted_points are special points. In this 
   sense, it's a  kind of inferitence relationship between an adjusted 
   point and a point. *)


(* immediate objects *)

let p =
  object
    val mutable x = 0
    method get_x = x
    method move d = x <- x + d
  end;;


p # get_x;;

p # move 3;;

(* Immediate objects can appear inside ordinary expressions
   and refer to variables from their cnntexts *)

let minmax x y =
  if x < y then object method min = x method max = y end
  else object method min = y method max = x end;;

let mm = minmax 1 2;;

mm # min, mm # max;;


(* object reference to self *)

class printable_point x_init =
  object (self)
    val mutable x = x_init
    method get_x = x
    method move d = x <- x + d
    method print = print_int self # get_x
  end;;

let p = new printable_point 7;;

p # print;;

(* 

let pc = new printable_point 'a';;

*)(* Won't work, with detectable type error*)

class adjusted_printable_point x_init = printable_point ((x_init / 10) * 10);;

let p = new adjusted_printable_point 23;;

p # print;;


let ints = ref [];;

let my_int =
  object (self)
    method n = 1
    method register = ints := self :: !ints
  end;;

my_int # register;;

ints;;

(* An immediate object can have a method to insert itself to 
   an external reference. While due to inheritence concerns, 
   this cannot be done by a class. *)

(* an initializer is an anomymous hidden method that is called
   immediately after an object is created *)
(* In contrast, a let-binding within a class definition but
   outside the "object...end" block is evaluated before an object
   is created *)

class printable_point x_init =
  let origin = (x_init / 10) * 10 in
  object (self)
    val mutable x = origin
    method get_x = x
    method move d = x <- x + d
    method print = print_int self # get_x
    initializer print_string "new point at " ;
      self # print; print_newline ()
  end;;

let p = new printable_point 17;;


(* virtual methods *)

(* We can declare a method without actually defining it, 
   with the mind to define it later in sub-classes -- 
   those that inherit. *)


