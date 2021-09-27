type ('p, 'f) app

module type Newtype1 = sig
  type 'a s
  type t
  external inj : 'a s -> ('a, t) app 
    = "%identity"
  external prj : ('a, t) app -> 'a s
    = "%identity"
end 

module Newtype1 (T : sig type 'a t end) : Newtype1 with type 'a s = 'a T.t = struct
  type t
  type 'a s = 'a T.t
  external inj : 'a -> 'b = "%identity"
  external prj : 'a -> 'b = "%identity"
end

type 'a perfect = Zero of 'a | Succ of ('a * 'a) perfect

module Perfect = Newtype1(struct type 'a t = 'a perfect end)

type 'f perfect_folder = {
    zero: 'a. 'a -> ('a, 'f) app;
    succ: 'a. ('a * 'a, 'f) app -> ('a, 'f) app;
  }

let folder : Perfect.t perfect_folder = let open Perfect in 
  {zero = (fun l -> inj (Zero l)) ;
   succ = (fun x -> inj (Succ (prj x)))}
                                        

let rec foldp : 'a. 'f perfect_folder -> 'a perfect -> ('a, 'f) app =
  fun {zero; succ} -> function
                   | Zero l -> zero l
                   | Succ p -> succ (foldp {zero;succ} p)
    
let idp p = let open Perfect in
            prj (foldp folder p)

let p1 = Succ(Succ(Zero((1,2),(3,4))))              
let p2 = idp p1
