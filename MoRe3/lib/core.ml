type 'a logic = Value of 'a | Var of Var.t

type 'a ilogic = 'a Term.t
        
external inj : 'a -> 'a ilogic = "%identity"
                             
let observe : Var.env -> 'a ilogic -> 'a logic =
  fun env t -> match Term.var env t with
               | None -> Value (Obj.magic t)
               | Some v -> Var v

module Env = struct
  type 'a t = Var.env -> 'a
  let return a = fun _ -> a
  let bind r k env = k (fun () ->  r () env)  env
end

module State = struct
  type 'a t = Var.env * 'a
end

module Reifier = struct
  type ('a, 'b) t = unit -> ('a -> 'b) Env.t
  let reify  = fun () -> observe
  let apply r (env, a) = r () env a
end
                   
let fresh g env = g (Term.fresh env) env

let run rel = let env = Var.fresh_env () in
              (env, rel (Term.fresh env) env)

let (>>=) = Env.bind


