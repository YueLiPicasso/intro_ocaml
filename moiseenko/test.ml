open Printf
open Core

let ret = Env.return

(* test that reifiyng a variable yeilds variable *)
let () =
  match Reifier.(apply reify @@ run (fun v -> ret v)) with
  | Var _   -> printf "Passed\n"
  | Value _ -> failwith "Failed"

(* test that reifiyng a fresh variable yeilds variable *)
let () =
  match Reifier.(apply reify @@ run (fun v -> fresh (fun x -> ret x))) with
  | Var _   -> printf "Passed\n"
  | Value _ -> failwith "Failed"

(* test that reifiyng a value yeilds value *)
let () =
  match Reifier.(apply reify @@ run (fun v -> ret @@ inj 42)) with
  | Value i ->
    if (i = 42) then
      printf "Passed\n"
    else
      failwith "Failed"
  | Var _   -> failwith "Failed"

(* test that runaway variables are handled *)
let () =
  let runaway : int ilogic ref = ref (Obj.magic ()) in
  let _ = run (fun v -> runaway := v; ret v) in
  try
    let _ = Reifier.(apply reify @@ run (fun v -> ret !runaway)) in
    failwith "Failed"
  with
  | Var_scope_violation v -> printf "Passed\n"

(* example of reifiers for custom types *)

module Option = struct

  type 'a t = 'a option

  type 'a ground = 'a t

  type 'a logic = 'a t Core.logic

  type 'a ilogic = 'a t Core.ilogic

  let fmap : ('a -> 'b) -> 'a t -> 'b t = fun f a ->
    match a with
    | Some a -> Some (f a)
    | None   -> None

  let reify : ('a, 'b) Reifier.t -> ('a ilogic, 'b logic) Reifier.t = fun ra ->
    let (>>=) = Env.bind in
    (Reifier.reify >>= (fun r -> (ra >>= (fun fa ->
      Env.return (fun x ->
        match r x with
        | Var v   -> Var v
        | Value t -> Value (fmap fa t)
      )
    ))))

end

(* test reification of the option *)

let () =
  match Reifier.(apply (Option.reify reify) @@ run (fun v -> ret @@ v)) with
  | Var _ -> printf "Passed\n"
  | _ -> failwith "Failed"

let () =
  match Reifier.(apply (Option.reify reify) @@ run (fun v -> ret @@ inj @@ Some v)) with
  | Value (Some (Var _)) -> printf "Passed\n"
  | _ -> failwith "Failed"

let () =
  match Reifier.(apply (Option.reify reify) @@ run (fun v -> ret @@ inj @@ Some (inj 42))) with
  | Value (Some (Value 42)) -> printf "Passed\n"
  | _ -> failwith "Failed"

(* example of the reifier for the custom recursive type *)

module List = struct

  type ('a, 't) t = Nil | Cons of 'a * 't

  type 'a ground = ('a, 'a ground) t

  type 'a logic = ('a, 'a logic) t Core.logic

  type 'a ilogic = ('a, 'a ilogic) t Core.ilogic

  let fmap : ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) t -> ('c, 'd) t = fun f g l ->
    match l with
    | Cons (a, b) -> Cons (f a, g b)
    | Nil         -> Nil

  let rec reify : ('a, 'b) Reifier.t -> ('a ilogic, 'b logic) Reifier.t = fun ra ->
    let (>>=) = Env.bind in
    (* Here the usage of `compose` is essential,
     * the 'monadic' implementation shown below fails with stack overflow due to an infinite recursion
     *)
    Reifier.compose Reifier.reify @@
      (ra >>= (fun fa -> (reify ra >>= (fun fr ->
        Env.return (fun lx ->
          match lx with
          | Var v   -> Var v
          | Value t -> Value (fmap fa fr t)
        )
      ))))

     (*    (Reifier.reify >>= (fun r -> (ra >>= (fun fa -> (reify ra >>= (fun fr ->*)
     (*      Env.return (fun x ->*)
     (*        match r x with*)
     (*        | Var v   -> Var v*)
     (*        | Value t -> Value (fmap fa fr t)*)
     (*      )*)
     (*    ))))))*)

  (* we can build reifier directly into `string`, that is nice *)
  let stringify : ('a, string) Reifier.t -> ('a ilogic, string) Reifier.t = fun sa ->
    let rec show f ll =
      match ll with
      | Var v   -> sprintf "_.%d" @@ Var.idx v
      | Value l ->
        begin match l with
        | Nil         -> "Nil"
        | Cons (a, t) -> sprintf "Cons (%s, %s)" (f a) (show f t)
        end
    in
    Reifier.compose (reify sa) (Env.return @@ show (fun s -> s))

end

(* test reification of the list *)

let () =
  let open List in
  match Reifier.(apply (List.reify reify) @@ run (fun v -> ret @@ v)) with
  | Var _ -> printf "Passed\n"
  | _ -> failwith "Failed"

let () =
  let open List in
  match Reifier.(apply (List.reify reify) @@ run (fun v -> ret @@ inj @@ (Cons (v, inj Nil)))) with
  | Value (Cons (Var _, Value Nil)) -> printf "Passed\n"
  | _ -> failwith "Failed"

let () =
  let open List in
  match Reifier.(apply (List.reify reify) @@ run (fun v -> ret @@ inj @@ (Cons (inj 42, inj Nil)))) with
  | Value (Cons (Value 42, Value Nil)) -> printf "Passed\n"
  | _ -> failwith "Failed"

(* perhaps, we can build `stringifiers` from the smaller pieces,
 * (i.g. by reusing function of type `('a -> string) -> 'a logic -> string`),
 * haven't thought about it a lot
 *)
let stringify : ('a -> string) -> ('a ilogic, string) Reifier.t = fun fs ->
  Reifier.(compose reify @@ Env.return (fun lx ->
    match lx with
    | Var v   -> sprintf "_.%d" @@ Var.idx v
    | Value x -> fs x
  ))

let () =
  let goal =
    let open List in
    run (fun tl -> fresh (fun x -> fresh (fun y ->
      ret @@ inj @@ (Cons (x, inj @@ Cons (inj 42, inj @@ Cons (y, tl))))
    )))
  in
  let s = Reifier.(apply (List.stringify @@ stringify string_of_int)) goal in
  printf "%s\n" s
