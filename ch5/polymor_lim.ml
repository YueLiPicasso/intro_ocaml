let store = ref None;;
(* store : '_weak1 option ref 
   where '_weak1 is a weak type variable, or 
   weakly polymorphic type variable *)

!store;; (* '_weak1 option *)
store := Some 2; !store;; (* Some 2 : int option*)
store;; (* int option ref *)
(* type error:
   store := Some 'a';; *)

(* A weak type variable is a place holder for 
   a single type that is currently unknown. Once the 
   type is known, it will be systematiclly substituted
   for '_weak.  *)

(* weal type variables allude to single-use polymorphism; 
   generic type variables allude to repeatable polymorphism.
*)

let swap store x = match !store with
  | None -> store := Some x; x
  | Some y -> store := Some x ; y;;


let store = ref None in
let v = swap store 1 in
v, swap store 2;;

store;; swap store 4;;
swap store 5;; !store;;

let store = ref None;; (* store: '_weak option ref*)
store := Some (fun x-> x);store;; (* store : ('_weak1 -> '_weak1) option ref *)
store := Some (fun x -> 5 - x);store;; (* store : (int -> int) option ref *)
store := Some (fun x -> x * 3);store;;
match !store with Some f -> f 4 | None -> 0;; 


let store = ref None;; (* store: '_weak option ref*)
store := Some (fun x-> true);store;; (* store : ('_weak1 -> bool) option ref *)
store := Some (fun x -> 5 < x);store;; (* store : (int -> bool) option ref *)
store := Some (fun x -> 6 >= x);store;;


let lrf = ref [];;
lrf := ['a'];lrf;;
(* type error: char list expected, string given 
lrf := "abcd";lrf;;
*)


let make_fake_id () = let store = ref None in
  fun x -> swap store x;;
let fake_id = make_fake_id();;

fake_id 2;;
(* type error: int expected, char given
fake_id 'a';;
*)

let cons = fun x y -> x ::  y;;
(cons 1 [2;3]);;
(cons 'a' ['b';'c']);;
(* cons has a generic polymorphic type: applying it to one type 
   does not fix its type to that type *)

swap (ref (Some 5)) 6;;
swap (ref (Some "str")) "ing";;
(* the type of swap is also genericly polymorphic *)

(* compare *)
let fake_id'  = let store = ref None in fun x -> swap store x;;
let fake_id'' = fun x -> let store = ref None in swap store x;;
fake_id' 2;; fake_id';;
fake_id'' 2;; fake_id'';;fake_id'' 'a';;
(* fake_id' has weakly polymorphic type but fake_id'' has generic 
   polymorphic type. The difference is made by move the 
   fun x -> part *)

let make_fake_id' () = fun x -> let store = ref None in  swap store x;;
make_fake_id'();;
let make_fake_id'' = fun x ~unit:() ->  let store = ref None in  swap store x;;
make_fake_id''~unit:();;
(* make_fake_id'() has weak polymorphic type, although
   the right hand side of the let-binding is the same as 
   that of the generic polymorphic function fade_id'' *)
(* make_fake_id''~unit:() has generic polymorphic type. 
   Removing the label or swaping the order of parameters 
   would cause it fall bak to weak polymorphism *)

let not_id = (fun x -> x)(fun x -> x);;
let id_again = fun x -> (fun x -> x)(fun x -> x) x;; 

(* relaxed value restriction: weak types at covariant position can be 
   safely generalized *)

let f () = [];;
let empty = f ();;


type x = [`X];;
type xy = [`X | `Y];;

(* x :> xy *)
let x : x = `X;;let x' = ( x :> xy );;
(* x list :> xy list*)
let l : x list = [`X;`X];;let l' = (l :> xy list);;
(* in 'a list, the parameter 'a is covariant, 
   since we have x :> xy and x list :> xy list *)

(* a function that handles values of type xy can also
   handle values of type x *)
let f : xy -> unit = function
  | `X | `Y -> ();;

let f' = (f :> x -> unit);;

(* the type constructor proc is contravariant at its parameter,
   since we have x :> xy and xy proc :> x proc *)
type 'a proc = 'a -> unit;;
let f' = (f : xy proc :> x proc);;

type 'b rtn = bool -> 'b;;
 
let g : x rtn =
  function true | false -> `X;;
let g' = (g : x rtn :> xy rtn);;
(* type constructor rtn is covariant at the parameter *)

(* 'a in 'a ref is invariant *)
let x : x ref = ref `X;;
(* type error
let xy = (x: x ref :> xy ref);;
*)

(* provide explicit variance information for abstract type *)


module Implementation :
sig
  type +'a t (* explicit variance annotation *)
  val empty: unit -> 'a t
end =
struct
  type 'a t = 'a list (* this equation is hidden by 
                         the signature, so IS the
                         variance of 'a in 'a t *)
  let empty () = []
end;;

Implementation.empty ();;

(* regular vs. non-regular recursive algebraic data type *)

module Regular =
struct
  type 'a nested = List of 'a list
                 | Nested of 'a nested list;;
  
  let rec maximal_depth = function
    | List _ -> 1
    | Nested [] -> 0
    | Nested (a :: q) ->
      1 + max (maximal_depth a)
        (maximal_depth (Nested q));;

  module Example =
    struct
      let l = List [1;2;3];;
      let ll = Nested [l;l];;
      let lll = Nested [ll;l];;
      let llll = Nested [l;ll;lll];;
    end;;
end;;

Regular.(maximal_depth Example.llll);;

module Non_regular =
struct
  type 'a nested = List of 'a list
                 | Nested of 'a list nested;;
  module Example =
  struct
    let l1 = List [1];; (* int nested *)
    let l2 = List [[1]];; (* int list nested *)
    let l3 = List [[[1]]];; (* int list list nested *)
    let l4 = List [[[[1]]]];; (* int list list list nested *)
    let l5 = List [[[[[1]]]]];; (* int list list list list nested *)
    
    let nl2 = Nested l2;; (* int nested *)
    let nl3 = Nested l3;; (* int list nested *)
    let nl4 = Nested l4;; (* int list list nested *)
    let nl5 = Nested l5;; (* int list list list nested *)
    
    let nnl3 = Nested nl3;; (* int nested *)
    let nnl4 = Nested nl4;; (* int list nested *)
    let nnl5 = Nested nl5;; (* int list list nested *)

    let nnnl4 = Nested nnl4;; (* int nested *)
    let nnnl5 = Nested nnl5;; (* int list nested *)

    let nnnnl5 = Nested nnnl5;; (* int nested *)
  end;;

  module Int_nested =
  struct
    open Example;;
    let v1 = l1
    and v2 = nl2
    and v3 = nnl3
    and v4 = nnnl4
    and v5 = nnnnl5;;
  end;;
end;;

Non_regular.Int_nested.(v1,v2,v3,v4,v5);;
