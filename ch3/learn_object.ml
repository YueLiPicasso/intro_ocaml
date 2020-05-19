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

(* A class that contains virtual methods must also 
   be declared virtual *)

class virtual abstract_point x_init =
  object (self)
    val mutable virtual x : int
    method virtual get_x : int
    method get_offset = self # get_x - x_init
    method virtual move : int -> unit
  end;;

(* new abstract_point 6;; *)
(* a virtual class cannot be instantiated *)

class point x_init =
  object 
    inherit abstract_point x_init as super
    val mutable x = x_init
    method get_x = x
    method move d = x <- x + d
  end;;

(* To inherit means to get all defined variables and methods, 
   while taking over the responsibility to implement all 
   virtual variables and methods, and with the right
   to add its own methods and variables. *)

(new point 13) # move 5;;

(* private methods *)
(* Private methods can only be invoked by methods from the same object *)

class restricted_point x_init =
  object (self)
    val mutable x = x_init
    method get_x = x
    method private move d = x <- x + d
    method bump = self # move 1
  end;;

let p = new restricted_point 0;;

(* p # move 10;; *) (* This gives error *)

p # bump;;
p # get_x;;

(* immediate object inheriting a class containing
   private methods *)
let p =
  object
    inherit restricted_point 10
  end;;

p # get_x;;
(* p # move;; *)(* causes error *)

(* inheritance, by a subclass, of a private method *)
class adjres_point x =
  let ori = (x / 10) * 10 in
  object
    inherit restricted_point ori
  end;;

let p = new adjres_point 24;;

p # bump;;
p # get_x;;
(* p # move 10;; *) (* error *)


(* we can make public an inherited private method *)
class point_again x =
  object 
    inherit restricted_point x
    method virtual move : _
    (* mention a method without providing its definition *)
  end;;

(* alternatively *)
class point_again x =
  object (self : < move : _ ; ..> ) (* requiring a public move method *)
    inherit restricted_point x
  end;;

class point_again x =
  object
    inherit restricted_point x as super
    method move = super # move
  end;;


(* compare *)

class point_again x =
  object
    inherit restricted_point x as super
    method pub_move = super # move
  end;;



(* Types and classes are independent in OCaml. For example, 
   the classes "point" and "adjusted point" produce objects of
   the same type *)


(* class interface *)

class type restricted_point_type =
  object
    method get_x : int
    method bump : unit
  end;;

fun (x : restricted_point_type) -> x;;

class restricted_point' x = (restricted_point x : restricted_point_type);;

(*
class restricted_point'' = (point : int -> restricted_point_type);;

Error: The class type int -> point is not matched by the class type
         int -> restricted_point_type
       The first class type has no method bump
       The public method get_offset cannot be hidden
       The public method move cannot be hidden


*)


p # get_x;; p # bump;;

class type point_type =
  object
    method get_x : int
    method move : int -> unit
    method get_offset : int
  end;;

class point' = (point : int -> point_type);;

(* a class type has its "must show", "must not show" 
   and "optinally show" *)

(* we can put a class interface into a module signature *)

module type POINT = sig
  class restricted_point' : int ->
    object
      method get_x : int
      method bump : unit
    end
end;;


module Point : POINT = struct
  class restricted_point' = restricted_point
end;;

let p = new Point.restricted_point' 12;;

p # get_x;; p # bump;;


(* inheritance *)

class colored_point x (c : string) =
  object
    inherit point x
    val c = c
    method color = c
  end;;

let p' = new colored_point 5 "red";;

p' # get_x , p' # color ;;

(* generic function *)

let get_succ_x p = p # get_x + 1;;
(* p is any object that has the method "get_x" *)

let p = new point 10;;

get_succ_x p + get_succ_x p';;


(* a genric function on objects can refer to methods
   that are not declared so far *)
let set_x p = p # set_x;;
(* Distinguish : set_x as a method of some object p, and set_x as a 
   top level function. These two interpretation of the same 
   name have distinct type. *)
(* The top level set_x is polymorphic in the class of its 
   arguments: it accpets any class that provides a method 
   named set_x. *)

let incr p = set_x p (get_succ_x p);;

class printable_colored_point y c =
  object (self)
    val c = c
    method color = c
    inherit printable_point y as super
    method! print =
      print_string "(";
      super # print;
      print_string ", ";
      print_string (self # color);
      print_string ")"
  end;;

let p' = new printable_colored_point 17 "red";;

p' # print;;

class another_printable_colored_point y c c' =
  object (self)
    inherit printable_point (y + 20)
    inherit! printable_colored_point y c
    val! c = c'
  end;;

let p = new another_printable_colored_point 17 "red" "green";;

p # print;;


(* val!, method! and inherit! are explit overriding annotation *)

(*
object
  method! m = ()
end;;
*)

(* parameterized class *)

(* Not allowed: 

class oref x_init =
  object
    val mutable x = x_init
    method get = x
    method set y = x <- y
  end;;

for at least one of the methods has a
   polymorphic type
*)

class oref (x_init : int) =
  object
    val mutable x = x_init
    method get = x
    method set y = x <- y
  end;;

           

(*An immediate object can be polymorphic *)    
let new_ored x_init =
  object
    val mutable x = x_init
    method get = x
    method set y = x <- y
  end;;

let stref =  new_ored "hello";;
let inref = new_ored 16;;

stref # get;; stref # set "world";;
inref # get;; inref # set 99;;

(* class polymorphism must be explict *)

class ['a] oref x_init =
  object
    val mutable x = (x_init : 'a)
    method get = x
    method set y = x <- y
  end;;

let r = new oref 1 in r # set 2 ; (r # get);;

(* a constraint may be imposed on the type variable
   of a polymorphic class *)

(* here 'a is constarined to int*)
class ['a] oref_succ (x_init : 'a) =
  object
    val mutable x = x_init + 1
    method get = x
    method set y = x <- y
  end;;

(* here 'a is constrained to any object that has a method
   move of type int -> unit *)
class ['a] circle (c : 'a) =
  object
    val mutable center = c
    method center = center
    method set_center c = center <- c
    method move = (center # move : int -> unit)
  end;;

(* here 'a is constrained to any object of class point 
   or a sub-class of point *)
class ['a] circle (c : 'a) =
  object
    constraint 'a = #point
    val mutable center = c
    method center = center
    method set_center c = center <- c
    method move = center # move
  end;;


let p' = new colored_point 5 "red";;
let c = new circle p';;

c # center # get_offset;;
c # move (-8);;

(* polymorphic class inheriance *)
class ['a] colored_circle c =
  object
    (*constraint 'a = #colored_point*)
    inherit ['a] circle c
    method color = center # color
  end;;
(* if we remove the explict constraint clause, then 
   the system can infer a suitable constraint on the
   type variable 'a: firstly it must an object of 
   (a subclass of ) the class point, and secondly it
   must provide a method color. We can see that an object
   of the class colored_point satisfies this inferred 
   constraint. *)


let c = new colored_circle p';;
let p = new colored_point 20 "black";;

c # color;;
c # set_center p;;
c # move 99;;
c # center # get_offset;; 


(* polymorphic methods *)

class ['a] intlist (l : int list) =
  object
    method empty = ( l = [])
    method fold f (accu : 'a) = List.fold_left f accu l
  end;;

(* this object is not polymorphic *)
(* Only its constructor is polymorphic *)
let l = new intlist [1;2;3];; 

(* the abstacrtion fixes the parameter type
   of the object l to int  *)
l # fold (fun x y -> x + y) 0;; 

l;;

let l = new intlist [1;2;3];; 
(* the abstraction fixes the parameter type of the 
   object l to string; we can change the argument 
   function, but the function's type must remain 
   string -> int -> string *)
l # fold (fun s x -> s ^ Int.to_string x ^ " ") "";;
l # fold (fun s x -> s ^ Int.to_string x ^ " hello ") "";; 


(* quantification is wrongly located; we want the class to be
   polymorphic in the way that the method fold of any object can 
   be used in a polymorphic manner. For this end we must give an
   explicit polymorphic type to the method definition, but not to 
   the class definition. *)

class intlist (l : int list) =
  object
    method empty = (l = [])
    method fold : 'a. ('a -> int -> 'a) -> 'a -> 'a =
      fun f accu -> List.fold_left f accu l
  end;;

(* now we can freely switch between arguments of different
   type *)
let l = new intlist [1;2;3];;
l # fold (fun x y -> x + y) 0;;
l # fold (fun x y -> x + y + 3) 0;;
l # fold (fun s x -> s ^ Int.to_string x ^ " ") "";;
l # fold (fun s x -> s ^ Int.to_string x ^ " hello ") "";; 

(* we can omit explicit polymorphic method type annotation 
   if we override such a method after inheritance *)

class intlist_rev l =
  object
    inherit intlist l
    method! fold f accu = List.fold_left f accu (List.rev l)
  end;;

let l = new intlist_rev [1;2;3];;
l # fold (fun s x -> s ^ Int.to_string x ^ " hello ") "";;
l # fold (fun x y -> x + y) 0;;


(* If we separate class interface from class definition, 
   quantification of polymorphic method type can be left 
   implicit *)

class type ['a] iterator =
  object method fold : ('b -> 'a -> 'b) -> 'b -> 'b end;;

class intlist' l =
  object (self : int #iterator)
    method empty = ( l = [] )
    method fold f accu = List.fold_left f accu l
  end;;


(* here is a specialized iterator description *)
class type iterate_intlist =
  object
    method empty : bool
    method fold :  ('a -> int -> 'a) -> 'a -> 'a
    (* or explicitly: method fold : 'a.('a -> int -> 'a) -> 'a -> 'a *)
  end;;


class intlist_again l =
  object (self : #iterate_intlist)
    method empty = ( l = [])
    method fold f accu = List.fold_left f accu l
  end;;

let l = new intlist_again [1;2;3];;
l # fold (fun s x -> s ^ Int.to_string x ^ " hello ") "";; 
l # fold (fun x y -> x + y) 0;;


(* This won't work:

class intlist_again l =
  object
    method empty = (l = [])
    method fold :  ('a -> int -> 'a) -> 'a -> 'a =
      fun f accu -> List.fold_left f accu l
  end;;

   The compiler would say that 'a is unbound *)

(* type variable quantification can be implicit in class descripion
   "class type ...", i.e., you don't need the " 'a. " prefix for the 
   type expression; however, you must add it in a class definition 
   "class ...". *)

let l = new intlist' [1;2;3];;

let sum lst = lst # fold (fun x y -> x + y) 0;;

(* Due to limitation of type inference, at the call site 
the polymorphic type of sum cannot be specialized by the type 
of its argument, therefore a type error is reported quotinng 
   "incompatible type" *)

(* sum l;; *)

(* To solve this problem, we can put a type constraint on
   the parameter *)

let sum (lst : _ # iterator) = lst # fold (fun x y -> x + y) 0;;

sum l;;

(* proide explicit method type as part of an object type *)

let sum lst =
  (lst : <fold : 'a. ('a -> _ -> 'a) -> 'a -> 'a; .. >) # fold ( + ) 0;;

sum l;;

(* a method can be polymorphic in the class of its arguments *)
(* we can achieve this using explicit quantification together
   with aliased type (typexpr as 'ident) *)
class type point0 = object method get_x : int end;;

class distance_point x =
  object
    inherit point x
    method distance : 'a. (#point0 as 'a) -> int =
      fun other -> abs (other # get_x - x)
  end;;

let p = new distance_point 3 in
(p # distance (new point 8), p # distance (new colored_point 1 "blue"));;


(* Another way to let methods be polymorphic in the class of
   its argument, is to specify the arg type not using #-type
   but using object type <method-type {; method-type}[;|;..]> *)

(* o is any object that has exactly one public method mtd : int *)
let f (o : < mtd : int>) = ();;

f (object method mtd = 6 end);;

let ob =  object (self)
  val x = 99
  method private get_x = x
  method mtd = self # get_x + 2 (* get_x can only be accessed through self*)
  initializer print_int x
end in f ob;;

(* here o is any object that at least has a 
   public method mtd : int *)
let f' (o : < mtd : int;..>) = ();;

f' (object method mtd = 6 method mtd' = 5 end);;

class multi_poly =
  object
    method m1 : 'a. (<n1 : 'b. 'b -> 'b; ..> as 'a) -> _ =
      fun o -> o # n1 true, o # n1 "hello"
    method m2 :  'b 'a.(<n2 : 'b -> bool;..> as 'a) -> 'b -> _ =
      fun o x -> o # n2 x
   (*  method m3 : 'b. <n2 : 'b -> bool;..> -> 'b -> _ =
       fun o x -> o # n2 x *)
      (* error : unbound type variable. <n2: 'b -> bool;..> 
         is regarded as a type variable, and it is not unbound 
         explicily. The way to bind it is to use an alias type
         named 'a. So for #point0 *)
  end;;


let ob1 = object
  method n2 = function
      "hello" -> true
    | "world" -> false
    | _ -> assert false
end;;

let ob2 = object
  val mutable x = 0
  method n2 y = if y >= 0 then (x <- y; true) else false
end;;

let ob3 = new multi_poly;;

ob3 # m2 ob1 "world", ob3 # m2 ob2 5;;


(* Using coercions *)

let p = new point 3 and q = new colored_point 4 "blue";;

(*

The type point and colored_point are not compatible, in the sense
that they are not regarded as the same type. Putting them in the 
same list produces a type error
 
[p;q];;
*)

(* explicit subtyping *)
(* i.e. identfy a type with its subtype *)
let colored_point_to_point cp = (cp : colored_point :> point);;

(* type coercion hides the methods unique for the sub-class*)
[p; (colored_point_to_point q)];;

(*
( p : point :> colored_point );; *)

(* subtyping does not necessarily result from 
   explicit inheritance: it just recognises the 
   subset relation between the two object types *)

class fruit id =
  object
    val name : string = id
    method name = name
  end;;

class apple id t =
  object
    val name : string = id
    method name = name
    val taste : string = t
    method taste = taste
  end;;

let apple_is_fruit a = (a : apple :> fruit);;

let a = new apple "apple" "sore" and f = new fruit "banana" in
[apple_is_fruit a;f];;


(* broadening coercion without explicit domain, and only 
   the codomain is specified *)

let to_point cp = (cp :> point);;


(* method duplicates self *)
class banana =
  object
    val  length : int = 4
    val color : string = "green"
    method reproduce = {<length=length + 1>}
    method clone = {<>}
    method size = length
    method color = color
  end;;

let b = new banana;;
let b' = b # reproduce;;
let b'' = b' # reproduce;;

[b # size, b # color; b'#size,b'#color;b''#size,b''#color];;

let b_1 = b # clone;;
let b_2 = b_1 # clone;;
let b_2' = b_2 # reproduce;;

[b # size, b # color;b_1#size,b_1#color;
 b_2#size,b_2#color;b_2'#size,b_2'#color];;

(* limitation of implicit-domain coercion *)

class c0 = object method m = {<>} method n = 0 end;;
(*
c0 = < m : 'a; n : int> as 'a 
*)

class type c1 = object method m : c1 end;;
(*
c1  = <m : c1>
#c1 = <m : c1; .. > 
*)

class type c2 = object ('a) method m : 'a end;;
(*
c2  = <m : 'a> as 'a
#c2 = <m : 'a; .. > as 'a 
*)

let ob = new c0;;

(* coercion from c0 to c1 and from c0 to c2 both work
   under explit domain *)

let c0_to_c1 = fun (x : c0) -> (x : c0 :> c1);;
let c0_to_c2 = fun (x : c0) -> (x : c0 :> c2);;

c0_to_c1 ob, c0_to_c2 ob;;

(* under implicit domain, only coercion from c0 to c2 works,
   but not from c0 to c1 *)

let to_c1 x = ( x :> c1);;
let to_c2 x = (x :> c2);;

(* to_c1 ob;; *)
to_c2 ob;;

(* functional objects *)

class functional_point y =
  object
    val x = y (* immutable instance variable *)
    method get_x = x
    method move d = {< x = x + d >} (* copy object *)
    method move_to x = {< x >} (* copy object *)
  end;;


let p = new functional_point 7;;

p # get_x;;

(p # move 3) # get_x;;

(p # move_to 15) # get_x;;

(* fresh objects cloned. p is not modified *)

p # get_x;;

(* compare with: *)

class bad_functional_point y =
  object
    val x = y
    method get_x = x
    method move d = new bad_functional_point (x+d)
    method move_to x = new bad_functional_point x
  end;;

let q = new bad_functional_point 7;;

q # get_x;;

(q # move 5) # get_x;;

(q # move_to 44) # get_x;;

q # get_x;;

class functional_colored_point y (s : string) =
  object
    inherit functional_point y
    val color = s
    method color = color
  end;;


class bad_functional_colored_point y (s : string) =
  object
    inherit bad_functional_point y
    val color = s
    method color = color
  end;;

let cp = new functional_colored_point 10 "red";; 

cp # get_x, cp # color;;      
(cp # move 6) # color, (cp # move_to 6) # color;;

let cq = new bad_functional_colored_point 55 "green";;

cq # get_x, cq # color;;
cq # move 5;;

(* since inheritance is just a syntactic relation, the move method 
of the bad subclass creates objects of the superclass, while that of 
the good subclass creates objects of the subclass itself *)

(* Cloning objects *)

(* object clone is shallow: if an instance variable refers to
   a reference cell or array, then the same reference cell or array 
   is shared between the object and its shallow clone, so that
   modification of the reference cell or array from one object 
   is visible from the perspective of the clone *)

let ob = Oo.copy
    (object
      val mutable cell = ref 5
      val arr = [|true;true;true|]
      method get_cell = !cell
      method set_cell n = cell := n
      method get_arr id = arr.(id)
      method set_arr id bo = arr.(id) <- bo
      method set x = cell <- x
    end);;

let ob' = Oo.copy ob;;

(* group 1 *)

ob # get_cell, ob' # get_cell;; (* (5,5) *)

ob' # set_cell 10;;

ob # get_cell, ob' # get_cell;; (* (10,10) *)

(* group 2 *)

ob # get_arr 0,ob # get_arr 1,ob # get_arr 2;; (* (true, true, true) *)
ob' # get_arr 0,ob' # get_arr 1,ob' # get_arr 2;; (* (true, true, true) *)

ob' # set_arr 0 false;
ob' # set_arr 1 false;
ob' # set_arr 2 false;;

ob # get_arr 0,ob # get_arr 1,ob # get_arr 2;;  (* (false, false, false) *)
ob' # get_arr 0,ob' # get_arr 1,ob' # get_arr 2;; (* (false, false, false) *)

(* group 3 *)

ob # set (ref 100);;
ob # get_cell, ob' # get_cell;; (* 100, 10 *)

Oo.copy;;

ob = ob';; (* for ojects, = iff == *)
ob <> ob';;

(* clone and override are interchangeable when 
   there is no instance variable update from the 
   override *)

(* the following two classes have the same object type *)

class copy =
  object
    method copy = {<>}
  end;;

class copy =
  object (self)
    method copy = Oo.copy self
  end;;

(* object state saving and restoring can be implemented
   using object clone *)

class backup =
  object (self)
    val mutable copy = None
    method save = copy <- Some {< copy = None >}
    method restore = match copy with
        Some x -> x | None -> self
  end;;


let ob = new backup;;
ob # restore = ob;; (* true *)
ob # save;;
ob # restore = ob;; (* false *)

(* using multiple inheritance, the backup facility 
   can be added to any class *)

class ['a] backup_ref x =
  object
    inherit ['a] oref x
    inherit backup
  end;;

(* experimenting with a  backup_ref object *)

let p = new backup_ref 0;;
p # get;; (* 0 *)
p # set 5;;
p # save;;
p # set 100;; 
p # get;; (* 100 *)
(p # restore) # get;; (* 5 *)
(p # restore) # set 100;;
(p # get) = (p # restore) # get && p # get = 100;; (* true *)
p # set 0;p # save;;
p # restore # get = 0;; (* true *)
p # restore # save;;
p # restore # restore # get;;
p # restore # restore # save;;
p # restore # restore # restore # get;;
p # restore # restore # restore # save;;

(* we see that iterative call of the "save" method from the restored object 
   creates a linked list *)


(* detour from the Ref. Man. :
   the backup_ref object can form a linked list *)

let rec deep_set p n m =
  if n = 0 then p # set m else deep_set (p # restore) (n-1) m;;


(*

deep_set p 0 k= p # set k
deep_set p 1 k= p # restore # set k
deep_set p 2 k= p # restore # restore # set k
deep_set p 3 k= p # restore # restore # restore # set k
...

*)

let rec deep_get_helper p n =
  if n = 0 then p # get else deep_get_helper (p # restore) (n-1);;

(* get the first n + 1 nodes' values from a linked list *)
let deep_get p n =
  let res = Array.make (n+1) 0 in
  for cnt = 0 to n do
    res.(cnt) <- deep_get_helper p cnt
  done;
  res
;;

let rec deep_save_helper p n =
  if n = 0 then p # save else deep_save_helper (p # restore) (n-1);;

(* create a linked lisi of n + 2 nodes *)
let deep_save p n =
  for cnt = 0 to n do
    deep_save_helper p cnt
  done;;

let size = 100;;
let p = new backup_ref 0;;

(* create a linked list of a give size *)
deep_save p size;;

(* set the values of nodes in the linked list *)
for cnt = 0 to size+1 do
  deep_set p cnt cnt
done;;

(* retrive the values from the linked list *)
deep_get p (size+1);;


