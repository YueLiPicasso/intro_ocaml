(* experiemnt 1 *)

module type X_int = sig val x : int end;;

module Increment (M : X_int) : X_int = struct
  let x = M.x + 1
end;;

module Three = struct let x = 3 end;;

module Four = Increment(Three);;

Four.x - Three.x;;

module Threep = struct
  let x = 3
  let y = "three"
end;;

module Four = Increment(Threep);;

Four.x - Threep.x;;

(* experiment 2 *)

(* Int minus : simplified Int library *)
module Intm = struct
  type t = int
  let compare = Int.compare
end;;

module type Comparable = sig
  type t
  val compare : t -> t -> int
end;;

module Mk (M : Comparable) = struct
  let notcomp : M.t -> M.t -> int = fun x y -> - (M.compare x y)   
end;;

module MkInt = Mk(Intm);;

module MkInt2 = Mk(struct type t = int let compare = Int.compare end);;

(* The inferred type for MkInt is 

module MkInt : sig val notcomp : Intm.t -> Intm.t -> int end

since the module Intm has no restrictive signature, it is known to the type checker
 that Intm.t = int
*)

MkInt.notcomp 5 9;; 

(* Functors must be applied to modules that satisfy the argument signature. 
A functor argument signature confines what the functor body knows about the argument
module, but does not limit what the type checker knows about the argument module, which
is determined by the module's own signature. *)

(* Error: values of type Intm.t = int is expected by the type checker 

MkInt.notcomp '1' '2';;

 *) 


(* The inferred type for MkInt2 is :

module MkInt2 : sig val notcomp : int -> int -> int end 

The argument is an anonymous module (of an arbitrary name) M, with M.t = int, but the 
arbitrary name M is not shown in the inferred type; instead, the type equation is 
applied to replace all occurrrences of M.t by int. 
*)


(* experiemnt 3 

Instantiating a functor can provide abstract types in the 
argument signature with concrete representations 
 *)

module type Data = sig
  type t
  val show : t -> string
end;;

module Fancy (M : Data) = struct
  let show : M.t -> string = fun d -> "Hello ! \n " ^ M.show d
end;;

module FInt = Fancy(struct type t = int let show = Int.to_string end);;

FInt.show 5;;

(*
module FStr = Fancy(struct type t = A | B | C let show = function A -> "A" | B -> "B" | C -> "C" end);;

Error: 

The parameter cannot be eliminated in the result type.
       Please bind the argument to a module identifier.
 *)

module AD = struct
  type t = A | B | C
  let show = function A -> "A" | B -> "B" | C -> "C"
end;;

module FAD = Fancy(AD);;

FAD.show A;;
