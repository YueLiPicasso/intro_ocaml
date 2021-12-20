type env = int

type index = int         

type t =
  { anchor : Anchor.t;
    env    : env;
    index  : index; }

let anchor { anchor; _ } = anchor
                      
let env { env; _ } = env
                
let index { index; _ } = index

let make ~env index = {env; index; anchor = Anchor.v}

let dummy = Obj.repr (make ~env:0 0)

let tag  = Obj.tag dummy
and size = Obj.size dummy            

let var_cnt = ref (-1)
            
let fresh_var_id () = incr var_cnt; !var_cnt
                    
let fresh env =  make ~env @@ fresh_var_id () 

exception Scope_violation of t     

let env_cnt = ref (-1)

let fresh_env () = incr env_cnt; !env_cnt  

(* the unit arg is necessary for expected result *)
