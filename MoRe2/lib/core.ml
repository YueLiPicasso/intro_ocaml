type 'a logic = Value of 'a | Var of Var.t

type 'a ilogic = 'a Term.t
        
external inj : 'a -> 'a ilogic = "%identity"
                             
let observe : Var.env -> 'a ilogic -> 'a logic =
  fun env t -> match Term.var env t with
               | None -> Value (Obj.magic t)
               | Some v -> Var v

module EL = struct
  type 'a t =
      Eager of 'a
    | Lazy  of 'a Lazy.t

  let use = function
    | Lazy f -> Lazy.force f
    | Eager f -> f
end

module Env = struct
  type 'a t = Var.env -> 'a

  let return a = fun _ -> a

  let bind r k env =
    let re = match r with
      | EL.Eager r ->  EL.Eager (r env)
      | EL.Lazy  r ->  EL.Lazy (lazy (Lazy.force r env))
    in k re env
end

module State = struct
  type 'a t = Var.env * 'a
end

module Reifier = struct
  type ('a, 'b) t = ('a -> 'b) Env.t EL.t
  let reify  = EL.Eager observe
  let apply r (env, a) =
    let r' = match r with
      | EL.Lazy  lr -> Lazy.force lr 
      | EL.Eager er -> er
    in r' env a
end
                   
let fresh g env = g (Term.fresh env) env

let run rel = let env = Var.fresh_env () in
              (env, rel (Term.fresh env) env)

let (>>=) = Env.bind

