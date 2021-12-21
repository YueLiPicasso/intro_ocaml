open Eucpp
open Eucpp.Core

type ('a, 'b) t = Nil | Cons of 'a * 'b

type 'a logic = ('a, 'a logic) t Core.logic

type 'a ilogic = ('a, 'a ilogic) t Core.ilogic

let fmap  =
  fun f g -> function
    | Nil -> Nil
    | Cons (h , t) -> Cons (f h ,  g t)

let rec reify = fun ra -> Reifier.compose Reifier.reify 
    (Env.bind ra (fun fa -> (Env.bind (reify ra) (fun fr ->
         Env.return (fun lx -> match lx with
             | Var _ as v' -> v'
             | Value t -> Value (fmap fa fr t))))))
    
(*
 List.reify ra env = fun xl -> match (Reifier.reify env xl) with
             | Var _ as v' -> v'
             | Value t -> Value (fmap (ra env)  (List.reify ra env) t) 
*)
