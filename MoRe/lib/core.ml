type 'a logic = Value of 'a | Var of Var.t

type 'a ilogic = 'a Term.t
        
external inj : 'a -> 'a ilogic = "%identity"
                             
let observe : Var.env -> 'a ilogic -> 'a logic =
  fun env t -> match Term.var env t with
               | None -> Value (Obj.magic t)
               | Some v -> Var v


module Env = struct
  type 'a t = Var.env -> 'a 
  let return a     = fun _ -> a
  let bind r k env = k (r env) env      
end

module State = struct
  type 'a t = Var.env * 'a
end

module Reifier = struct
  type ('a, 'b) t = ('a -> 'b) Env.t
  let reify = observe
  let apply r (env, a) = r env a
  module Lazy = struct
    let apply r (env, a) = apply (Lazy.force r) (env, a)
    let bind r k env = k (env, r) env
    let force (env, r) = Lazy.force r env
  end
end
                   
let fresh g env = g (Term.fresh env) env

let run rel = let env = Var.fresh_env () in
              (env, rel (Term.fresh env) env)

let (>>=) = Env.bind
let (>>>=) = Reifier.Lazy.bind 

