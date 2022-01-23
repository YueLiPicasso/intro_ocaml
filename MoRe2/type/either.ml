open More
open More.Core

type ('a, 'b) t = Left of 'a | Right of 'b

type ('a, 'b) logic = ('a, 'b) t Core.logic

type ('a, 'b) ilogic = ('a, 'b) t Core.ilogic

let left = fun v -> inj (Left v)

let right = fun v -> inj (Right v)

let fmap = fun ~left ~right -> function
  | Left v  -> Left (left v)
  | Right v -> Right (right v)

let reify = fun ~left ~right ->
  EL.Eager (Reifier.reify >>= (fun r -> (left >>= fun fl -> (right >>= (fun fr ->
      Env.return (fun x ->
          match EL.use r x with
          | Var _ as v -> v
          | Value t -> Value (fmap (EL.use fl) (EL.use fr) t)))))))
