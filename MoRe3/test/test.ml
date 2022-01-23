open More
open More.Core    
open Type

(* combinations of run, fresh, return *)
module Local_def : sig  
  val runrt : unit -> 'a ilogic State.t
  val runfr : unit -> 'a ilogic State.t  
end = struct
  let runrt = fun () -> run Env.return 
  let freshret = fun () -> fresh Env.return          
  let runfr = fun () -> run @@ fun _ ->  freshret ()
end

open Local_def
    
(* Reify variables *)

let _ = print_string @@ match Reifier.(apply reify @@ runrt ()) with
  | Value _ -> "failed\n"
  | Var _ -> "PASSED\n"
and _ = print_string @@ match Reifier.(apply reify @@ runfr ()) with
  | Value _ -> "failed\n"
  | Var _ -> "PASSED\n"
and _ = print_string @@
  match Reifier.(apply reify @@ run (fun _ -> Env.return @@ inj 44)) with
  | Value 44 -> "PASSED\n"
  | _ -> "failed\n"

let _ =
  let runaway : int ilogic ref = ref (Obj.magic ()) in  
  let state_ok = run (fun v -> runaway := v; Env.return v) in
  try Reifier.(apply reify @@ run (fun _ -> Env.return !runaway))   
               with
               | Var.Scope_violation _ -> print_string "PASSED\n";
                                          Reifier.(apply reify state_ok)
(* test fresh number generator *)
                                            
let _ = 
  print_string @@ Stdlib.List.fold_right
    (fun v s ->
       s ^ "(" ^
       string_of_int ((Obj.magic (Var.index v)) : int)
       ^
       " , "
       ^
       string_of_int ((Obj.magic (Var.env v))   : int)
       ^
       ")\n")
    Var.[fresh (fresh_env ());fresh (fresh_env ());
         fresh (fresh_env ());
         fresh (fresh_env ());fresh (fresh_env ());
         fresh (fresh_env ())]
    ""
(* [expr1 ; expr2; expr3 ; ... ; exprn] evalutation order 
   exprn , ..., expr3, expe2, expr1. *)

let cons = List.cons and nil = List.nil and some = Option.some and none = Option.none

(* reify options *)

let _= print_string @@ 
  let v : 'a Core.logic Option.logic =
    Reifier.apply (Option.reify Reifier.reify) (runrt()) in
  match v with
  | Var _ -> "PASSED\n"
  | _ -> "failed\n"

let _ = print_string @@
  let tm : 'a Core.logic Option.logic =
    Reifier.apply (Option.reify Reifier.reify)
      (run (fun v -> Env.return (some v))) in
  match tm with
  |  Value (Some (Var _)) -> "PASSED\n"
  | _ -> "failed\n"
    
let _ = print_string @@
  let tm : int Core.logic Option.logic =
    Reifier.apply (Option.reify Reifier.reify)
      (run (fun _ -> Env.return (some (inj 49)))) in
  match tm with
  |  Value (Some (Value 49)) -> "PASSED\n"
  | _ -> "failed\n"

let _ = print_string @@
  let tm : bool Core.logic Option.logic Option.logic  =
    Reifier.apply (Option.reify (Option.reify Reifier.reify))
      (run (fun _ -> Env.return (some (some (inj true))))) in
  match tm with
  |  Value (Some (Value (Some (Value true)))) -> "PASSED\n"
  | _ -> "failed\n"


(* Reify List.Rec *)

let _ = print_string @@
  let tm = Reifier.apply (List.Rec.reify Reifier.reify) (runrt()) in
  match tm with
  | Var _ -> "PASSED\n"
  | _ -> "failed\n"

let _ = print_string @@
  let tm = Reifier.apply (List.Rec.reify Reifier.reify)
      (run (fun v -> Env.return (cons v (nil())))) in
  match tm with
  | Value (Cons (Var _, Value Nil)) -> "PASSED\n"
  | _ -> "failed\n"

let _ = print_string @@
  let tm = Reifier.apply (List.Rec.reify Reifier.reify)
      (run (fun v -> Env.return
               (cons v (cons v (nil()))))) in
  match (tm : 'a Core.logic List.Rec.logic) with
  | Value(Cons(Var _, Value (Cons (Var _, Value Nil)))) -> "PASSED\n"
  | _ -> "failed\n"

let _ = print_string @@
  let tm = Reifier.apply (List.Rec.reify Reifier.reify)
      (run (fun v -> Env.return
               (cons (inj 5) (cons (inj 6) v)))) in
  match (tm : int Core.logic List.Rec.logic) with
  | Value(Cons(Value 5, Value (Cons (Value 6, Var _)))) -> "PASSED\n"
  | _ -> "failed\n"

(* reify option list *)
    
let _ = print_string @@
  let tm = Reifier.apply (List.Rec.reify (Option.reify Reifier.reify))
      (run (fun v -> Env.return
               (cons (some (inj 5)) (cons v (nil()))))) in
  match (tm : int Core.logic Option.logic List.Rec.logic) with
  | Value(Cons(Value(Some(Value 5)), Value (Cons (Var _, Value Nil))))
    -> "PASSED\n"
  | _ -> "failed\n"


let _ = print_string @@
  let tm = Reifier.apply (List.Rec.reify (Option.reify Reifier.reify))
      (run (fun v -> Env.return
               (inj List.(Cons(inj (Some v),
                               inj (Cons(inj None, (inj Nil)))))))) in
  match (tm : int Core.logic Option.logic List.Rec.logic) with
  | Value(Cons(Value(Some(Var _)), Value (Cons (Value None, Value Nil))))
    -> "PASSED\n"
  | _ -> "failed\n"

(* reifier not deep enough in general but deep enough for the special case *)

let _ = print_string @@
  let (tm : Option.Rec.ilogic Option.logic Option.logic List.Rec.logic)
    = Reifier.apply (List.Rec.reify (Option.reify Reifier.reify))
      ((run (fun v -> Env.return
                (cons (some v) (cons v (nil())))))
       :  Option.Rec.ilogic List.Rec.ilogic State.t) in
  match tm with
  | Value(Cons(Value(Some(Var _)), Value (Cons (Var _, Value Nil))))
    -> "PASSED\n"
  | _ -> "failed\n"

(* partial reification using a not-deep-enough reifier *)

let _ = print_string @@
  let (tm : (Option.Rec.ilogic Option.logic Option.logic List.Rec.logic)) =
    Reifier.apply (List.Rec.reify (Option.reify Reifier.reify))
      ((run (fun v -> Env.return
               (inj List.(Cons(inj (Some (inj (Some (inj (Some v))))),
                               inj (Cons(v, (inj Nil))))))))
    :  Option.Rec.ilogic List.Rec.ilogic State.t) in
  match tm with
  | Value(Cons(Value(Some(Value(Some _))), Value (Cons (Var _, Value Nil))))
    -> "PASSED\n"
  | _ -> "failed\n"

(* How to get two vars of different but correct type? *)

let _ = print_string @@
  match Reifier.apply (List.Rec.reify Reifier.reify)
          (run (fun x -> fresh (fun y -> Env.return @@ List.cons x y))) with
  | Value(Cons(Var v1, Var v2))
    when Var.(index v1 <> index v2) && Var.(env v1 = env v2) -> "PASSED\n"
  | _ -> "failed"

let _ = print_string @@
  match Reifier.apply
          (List.Rec.reify
             (Option.reify
                (Reifier.reify : (int Core.ilogic, int Core.logic) Reifier.t)))
          (run (fun x ->
               fresh (fun y ->
                   Env.return
                     List.(cons (Option.some y)
                             (cons x (nil()))))))
  with
  | Value(Cons(Value(Some(Var v1)), Value(Cons(Var v2, Value Nil))))
    when Var.(index v1 <> index v2) && Var.(env v1 = env v2) -> "PASSED\n"
  | _ -> "failed\n"

let _ = print_string @@
  match Reifier.apply
          (List.Rec.reify
             (Option.reify
                (Reifier.reify : (int Core.ilogic, int Core.logic) Reifier.t)))
          (run (fun v1 ->
              fresh (fun v2 ->
                fresh (fun v3 ->
                  Env.return
                    List.(cons v1
                            (cons (Option.some v2)
                               (cons (Option.none()) v3)))))))
  with
  | Value(Cons(Var v1,
               Value(Cons(Value(Some(Var v2)),
                          Value(Cons(Value None, Var v3))))))
    when Var.(index v1 < index v2) && Var.(index v2 < index v3) -> "PASSED\n"
  | _ -> "failed\n"
    
(* reify finitely nested options *)

let _ = print_string @@
  let (tm : Option.Rec.logic list)
    = Stdlib.List.map (Reifier.apply Option.Rec.reify)
      ([(run Env.return);
        (run (fun _ -> Env.return Option.(none())));
        (run (fun _ -> Env.return Option.(some (none()))));
        (run (fun _ -> Env.return Option.(some (some (none())))));
        (run (fun _ -> Env.return Option.(some (some (some (none()))))));
        (run (fun _ -> Env.return Option.(some (some (some (some (none())))))))] 
       :  Option.Rec.ilogic State.t list ) in
  match tm with
  | [Var _ ;
     Value None;
     Value(Some(Value None));
     Value(Some(Value(Some(Value None))));
     Value(Some(Value(Some(Value(Some(Value None))))));
     Value(Some(Value(Some(Value(Some(Value(Some(Value None))))))))]
    -> "PASSED\n"
  | _ -> "failed\n"

(* reify infinitely nested options *)

let _ = print_string @@
  let tm : Option.Seq.logic = Reifier.apply Option.Seq.reify
      (run (fun _ -> Env.return @@ Option.Seq.oo))
  in
  begin
    match Option.take 1 tm with
    | Value(Some(Value None)) -> "PASSED\n"
    | _ ->  "failed\n"
  end
  ^
  begin
    match Option.take 2 tm with
    | Value(Some(Value(Some(Value None)))) -> "PASSED\n"
    | _ ->  "failed\n"
  end
  ^
  begin
    match Option.take 3 tm with
    | Value(Some(Value(Some(Value(Some(Value None)))))) -> "PASSED\n"
    | _ ->  "failed\n"
  end
  

(* reify list of finitely nested options *)

let _ =
  print_string @@
  let (tm : (Option.Rec.logic List.Rec.logic))
    = Reifier.apply (List.Rec.reify Option.Rec.reify)
      ((run (fun v -> Env.return
                (cons (some v) (cons v (cons (some (some (some v))) (nil()))))))
       :  Option.Rec.ilogic List.Rec.ilogic State.t) in
  match tm with
  | Value
      (Cons(Value(Some(Var _)),
            Value (Cons (Var _,
                         Value(Cons(Value(Some(Value(Some(Value(Some(Var _)))))),
                                    Value Nil))))))
    -> "PASSED\n"
  | _ -> "failed\n"

(* reify list of list of finitely nested options *)

(*[[Some None];
   [Some(Some(Some None)); Some(Some v)]] *)
let _ =
  print_string @@
  let (tm : Option.Rec.logic List.Rec.logic List.Rec.logic) =
    Reifier.apply (List.Rec.reify (List.Rec.reify Option.Rec.reify))
      (run (fun v -> Env.return (cons (cons (some (none())) (nil()))
                                   (cons (cons (some(some(some (none()))))
                                            (cons (some(some v)) (nil()))) (nil())))))
  in
  match tm with
  | Value(Cons(Value(Cons(Value(Some(Value None)),Value Nil)),
               Value(Cons(Value(Cons(Value(Some(Value(Some(Value(Some(Value None)))))),
                                     Value(Cons(Value(Some(Value(Some(Var _)))),Value Nil)))),
                          Value(Nil)))))
    -> "PASSED\n"
  | _ -> "failed\n"
    
(* reify option of integer list *)
    
let _ = print_string @@
  let (tm : int Core.logic List.Rec.logic Option.logic) =  
    Reifier.apply (Option.reify (List.Rec.reify Reifier.reify))
      (run (fun v -> Env.return (some (cons v (cons (inj 1) (nil()))))))
  in match tm with
  | Value(Some(Value(Cons(Var _, Value(Cons(Value 1, Value Nil)))))) -> "PASSED\n"
  | _ -> "failed\n"

(* reify either *)

let left = Either.left and right = Either.right

let _ = print_string @@
  let (tm : ('a, 'b) Either.logic) =  
    Reifier.apply (Either.reify Reifier.reify Reifier.reify)
      (run (fun v -> Env.return (left v)))
  in match tm with
  | Value(Left(Var _)) -> "PASSED\n"
  | _ -> "failed\n"

let _ = print_string @@
  let (tm : (('a, 'b) Either.logic, ('c, 'd) Either.logic) Either.logic) =  
    Reifier.apply (Either.reify
                     (Either.reify Reifier.reify Reifier.reify)
                     (Either.reify Reifier.reify Reifier.reify))
      (run (fun v -> Env.return (right (left v))))
  in match tm with
  | Value(Right(Value(Left(Var _)))) -> "PASSED\n"
  | _ -> "failed\n"

let _ = print_string @@
  let (tm : ('a Option.logic, 'a List.Rec.logic) Either.logic) =  
    Reifier.apply (Either.reify
                     (Option.reify Reifier.reify)
                     (List.Rec.reify Reifier.reify))
      (run (fun v -> Env.return (right (cons v (nil())))))
  in match tm with
  | Value(Right(Value(Cons(Var _, Value Nil)))) -> "PASSED\n"
  | _ -> "failed\n"

(* reify infinite list *)

let _ = print_string @@
  let tm : int Core.logic List.Seq.logic = Reifier.apply (List.Seq.reify Reifier.reify)
      (run (fun _ -> Env.return @@ List.Seq.ints 0))
  in
  begin
    match List.take 1 tm with
    | Value(Cons(Value 0, Value Nil)) -> "PASSED\n"
    | _ ->  "failed\n"
  end
  ^
  begin    match List.take 2 tm with
    | Value(Cons(Value 0, Value(Cons(Value 1, Value Nil)))) -> "PASSED\n"
    | _ ->  "failed\n"
  end
  ^
  begin    match List.take 3 tm with
    | Value(Cons(Value 0, Value(Cons(Value 1, Value(Cons(Value 2, Value Nil)))))) ->
      "PASSED finally\n"
    | _ ->  "failed\n"
  end
  
