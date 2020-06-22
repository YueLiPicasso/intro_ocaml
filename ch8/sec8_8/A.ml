let rec app l1 l2 =
  match l1 with
  | [] -> l2
  | h :: t -> h :: app t l2
