open OCanren;;
open Trie;;

let _ = print_string @@ (GT.show(ground) @@ prj inito) ^ "\n";;

let _ = print_string @@ (fun x -> GT.show(logic) x ^ "\n") @@
  List.hd @@ RStream.take @@ run q (fun q -> ocanren { q == inito }) (fun q -> q#reify(reify));;
