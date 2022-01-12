open More
open More.Core

[@@@ocaml.warning "-34-32"]

type 'a t = Z | S of 'a

type logic = logic t Core.logic

type ilogic = ilogic t Core.ilogic

let z () = inj Z

let succ x = inj @@ S x

let fmap f = function Z -> Z | S t -> S (f t)

let rec reify =
  lazy
    ( Reifier.reify >>= fun r ->
      reify >>>= fun fr ->
      Env.return (fun x ->
          match r x with
          | Var _ as v -> v
          | Value t -> Value (fmap (Lazy.force fr) t)) )

let reify_strict = Lazy.force reify

module Nat2 = struct
  type 'a t = Z | S of 'a

  type 'a logic = 'a t Core.logic

  type 'b ilogic = 'b t Core.ilogic

  let z () = inj Z

  let succ x = inj @@ S x

  let fmap f = function Z -> Z | S t -> S (f t)

  (* A reifier in open recursion style *)
  let reify' : ('a -> 'b) Env.t lazy_t -> ('a ilogic -> 'b logic) Env.t lazy_t =
   fun ra ->
    lazy
      ( Reifier.reify >>= fun r ->
        ra >>>= fun fa ->
        Env.return (fun x ->
            match r x with
            | Var _ as v -> v
            | Value t -> Value (fmap (Lazy.force fa) t)) )

  let reify_strict fa = Lazy.force (reify' (lazy fa))

  (* How to get a reifier like in line 18 using open recursion reifier? *)
  (*
  let reify_tie =
    let rec self = lazy (reify' self) in
    Lazy.force self
    *)
end
