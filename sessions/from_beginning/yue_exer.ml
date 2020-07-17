
let rec smallest l =
  let rec posMem l p =
  match l with
    [] -> p
  | h :: t -> if h > 0 then posMem t (h :: p) else posMem t p in
  let pos = posMem l [] in
  match pos with
    [] -> raise Not_found
  | [n] ->  n 
  | h1 :: h2 :: t -> if h1 > h2 then smallest (h2 :: t)
    else smallest (h1 :: t);;


let smallest_or_zero l =
  try smallest l with Not_found -> 0;;

smallest_or_zero [-1;-2;-3;-1;0;-2];;

let rec key_exists k d =
  try
    let _ = List.assoc k d in true
  with
  Not_found -> false;;

key_exists 3 [(1,2);(2,3)];;

let make_dic l =
  let rec make_dic_keep_last l =
    match l with
      [] -> []
    | ((k, _) as en) :: tl -> if key_exists k tl then make_dic_keep_last tl
      else en :: make_dic_keep_last tl in
  let tmp = make_dic_keep_last (List.rev l) in
  List.rev tmp;;

let union a b =
  make_dic (a @ b);;


union [1,2;2,3] [1,5;2,5;3,4];;
List.rev [1;2;3];;


let mapll f = List.(map (map (map f)));;

mapll (( * ) 2) [[[1;2;3];[4;5;6];[7;8;9]];
                 [[10;11;12];[13;14;15];[16;17;18]];
                 [[19;20;21];[22;23;24];[25;26;27]]];;

let x = 1 in let x = x + 1 in x;;
