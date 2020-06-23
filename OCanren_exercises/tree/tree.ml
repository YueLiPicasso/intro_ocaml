open Printf;;
open GT;;
open OCanren;;
open OCanren.Std;;


module Tree = struct
  module X = struct
    @type ('a, 'self) t = Leaf | Node of 'a * 'self * 'self with gmap, show;;
    let fmap eta = GT.gmap t eta;;
  end;;

  include X;;
  include Fmap2(X);;

  @type inttree = (int , inttree) X.t with show;;

  @type rtree = (LNat.ground, rtree) X.t with show;;
          
  @type ltree = (LNat.logic, ltree) X.t logic with show;;

  type ftree = (rtree, ltree) injected;;

  let leaf () : ftree = inj @@ distrib @@ X.Leaf;;
  let node a b c : ftree = inj @@ distrib @@ X.Node (a,b,c)

  let rec inj_tree : inttree -> ftree = fun tree ->
    inj @@ distrib @@ GT.(gmap t nat inj_tree tree);;

  let rec prj_tree : rtree -> inttree =
    fun x -> GT.(gmap t) LNat.to_int prj_tree x;;

end;;


open Tree;;

let rec inserto a t' t'' = conde [
    (t' === leaf ()) &&& (t'' === node a (leaf()) (leaf ()) );
    fresh (x l r l')
      (t' === node x l r)
      Nat.(conde [
          (t'' === t') &&& (a === x);
          (t'' === (node x l' r)) &&& (a < x) &&& (inserto a l l');
          (t'' === (node x l l')) &&& (a > x) &&& (inserto a r l')
        ])
  ];;


(* wrapping the relational core in an OCaml function *)

let insert : int -> inttree -> inttree = fun a t ->
  prj_tree @@ RStream.hd @@
  run q (fun q -> inserto (nat a) (inj_tree t) q)
    (fun qs -> qs#prj);;

let insert' t t' =
  LNat.to_int @@ RStream.hd @@
  run q (fun q -> inserto q (inj_tree t) (inj_tree t'))
    (fun qs -> qs#prj);;

let insert'' x t' =
  prj_tree @@ RStream.hd @@
  run q (fun q -> inserto (nat x) q (inj_tree t'))
    (fun qs -> qs#prj);;

let _ =
  let insert_list l =
    let rec inner t = function
      | [] -> t
      |  x :: xs ->
        let t' = insert x t in
        printf "Inserting %d into %s makes %s\n%!" x
          (show_inttree t) (show_inttree t');
        inner t' xs
    in
    inner Leaf l
  in
  ignore @@ insert_list [1;2;3;4];
  let t = insert_list [3;2;4;1] in
  let t' = insert 8 t in
  Printf.printf "Inverse insert: %d\n" @@ insert' t t';
  Printf.printf "Removing %d from %s results in %s\n%!"
    4 (show_inttree t) (show_inttree (insert'' 4 t));;



(* inject values to logic domain, then project it out and print it *)

let lb = inj @@ lift true in
Stdlib.(print_string (string_of_bool (prj lb));
print_newline ());;

let ls = inj @@ lift "abc" in
Stdlib.(print_string (prj ls);
print_newline ());;

let l2 = (!!) 2 in
Stdlib.(print_int (prj l2);
print_newline ());;


