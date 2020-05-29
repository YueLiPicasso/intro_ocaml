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
      let l = List [1;2;3];; (* int nested *)
      let ll = Nested [l;l];; (* int nested *)
      let lll = Nested [ll;l];; (* int nested *)
      let llll = Nested [l;ll;lll];; (* int nested *)
    end;;
end;;

Regular.Example.(l,ll,lll,llll);;

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

  let rec depth : 'a. 'a nested -> 'b = function
    (* the type variable 'b (unbound and named) can be 
       replaced by _ (anonymous type variable) or int *)
    (* the scope of 'b is this let-expression, i.e., the 
       top level phrase in which it appears *)
    | List _ -> 1
    | Nested n -> 1 + depth n;;

  (* the 'b in the type constraint of f is not the same type 
     variable as the 'b in the type constraint of depth, so they 
     can be instantiated into incompatible types int and bool *)
  let f : 'b -> bool = fun x -> x && true;;

  let map_and_sum f = List.fold_left (fun acc x -> acc + f x) 0;;  
  (* Let [a1;a2;...;an] be any list, and f : 'a -> int. Then 
     map_and_sum f [a1;a2;...;an] evaluates to 
     (f a1) + (f a2) + ... (f an) 
  *)

  (* the map_and_sum function is a special case of 
     a general combination of map-and-fold *)
  module type MAP =
  sig
    val foldl :
      mapf:('a -> 'b) ->
      foldf:('c -> 'b -> 'c) ->
      init:'c ->
      'a list -> 'c
  end;;

  module Map : MAP =
  struct
    let foldl ~mapf ~foldf ~init =
      fun l -> List.fold_left foldf init (List.map mapf l)
  end;;

  let map_and_sum' ~mapf = Map.foldl ~mapf ~foldf:( + ) ~init:0;;

  let rec nested_length : 'a. ?len:('a list -> int) -> 'a nested -> int =
    fun ?len:(lenf = List.length) -> 
    function | List l -> lenf l
             | Nested n -> nested_length ~len:(map_and_sum' ~mapf:lenf) n;;
  
  (* type inference by hand gives the poly-typexpr with _ *)
  let rec shape
  : 'a 'b. ('a nested -> _ ) -> ('b list list -> 'a list) -> 'b nested -> _
  = fun nest nested_shape nl ->
    match nl with
    | List l -> raise (Invalid_argument "list too shallow")
    | Nested (List l) -> nest @@ List (nested_shape l)
    | Nested n -> let nested_shape' = List.map nested_shape 
      and nest' x = nest (Nested x) in
      shape nest' nested_shape' n;;

  let nshape : 'a nested -> 'c =
    fun n -> shape (fun x -> x) (fun l -> List.map List.length l) n;;
end;;


Non_regular.map_and_sum' ~mapf:(fun b -> if b then 1 else 0) [true;true;true;false;true];;
Non_regular.Int_nested.(v1,v2,v3,v4,v5);;
List.map Non_regular.depth Non_regular.Int_nested.[v1;v2;v3;v4;v5];; 
(* The difference between 'a. 'a nested -> int and 'a nested -> int 
   reminds me of the explicit generalization of the inductive 
   hypothesis in Coq *)

(* 'a, 'b and 'c denote type variables that may or may not 
   be polymorphic, so they can be instantiated during type checking  *)
let sum : 'a -> 'b -> 'c = fun x y -> x + y;;

(* an explicitly polymorphic type cannot be unified with a 
   non-polymorphic type. The following expression is rejected: *)

(* let sum : 'a 'b 'c. 'a -> 'b -> 'c = fun x y -> x + y;; *)

(* The scope of the named type variable 'a is the whole top-level phrase.
   It represents an unspecified type that can be inistantiated by any type
   ttp satisfy the type constraint 

   let f : 'a -> int = fun x -> x + 5 and
   g : 'a -> bool = fun b -> b && true;;

   In this case the type variable is first instantiated by int when binding
   f and then when type checking g, b is supposed to be an int and a type 
   clash appeared because a bool is expected.   
*)


(* Number of elements in a nested list can be calculated by mappiing 
   the length function *)

(* List *)
List.length [1;1;1];;

(* Nested List *)
List.fold_left ( + ) 0
  (List.map List.length [[1];[1];[]]);;
(* or *)
Non_regular.map_and_sum' ~mapf:List.length  [[1];[1];[]];;

(* Nested Nested List *)
List.fold_left ( + ) 0
  (List.map (List.fold_left ( + ) 0)
     (List.map (List.map List.length) [[[1];[2]];[[];[3]];[[4];[]]]));;
(* or *)
let open Non_regular in
map_and_sum'
  ~mapf:(map_and_sum' ~mapf:List.length)
  [ [[1];[2]] ; [[];[3]] ; [[4];[]] ];;


(* Nested Nested Nested List *)
List.fold_left ( + ) 0
  (List.map (List.fold_left ( + ) 0)
     (List.map (List.map (List.fold_left ( + ) 0))
        (List.map (List.map (List.map List.length))
           [[[[1;2];[3;4]];[[5;6];[7;8]]];[[[2];[]];[]];[]])));;
(* or *)
let open Non_regular in
map_and_sum'
  ~mapf:(map_and_sum'
           ~mapf:(map_and_sum' ~mapf:List.length))
  [[[[1;2];[3;4]];[[5;6];[7;8]]];[[[2];[]];[]];[]];;

List.map Non_regular.nested_length Non_regular.Int_nested.[v1;v2;v3;v4;v5];; 


(* shape of a nested list *)

(* Nested List *)
List.map List.length [[1];[1];[]];;
(* Nested Nested List *)
List.map (List.map List.length) [[[1];[2]];[[];[3]];[[4];[]]];;
(* Nested Nested Nested List *)
List.map (List.map (List.map List.length))
           [[[[1;2];[3;4]];[[5;6];[7;8]]];[[[2];[]];[]];[]];;

Non_regular.(nshape Int_nested.v4);; 



