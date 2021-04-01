module type S =
  sig
    type t
  end;;

module A = struct
  type t = char
end;;

module B = struct
  type t = string
end;;

module F (M : S) =
  struct
    type ty = int
    type t = M.t
    type n
  end;;

module M1 = F(A);;
module M2 = F(B);;
