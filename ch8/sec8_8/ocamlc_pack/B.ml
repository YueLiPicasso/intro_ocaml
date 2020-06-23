let rec appr l1 l2 =
  match l2 with
  | [] -> l1
  | h :: t -> h :: appr l1 t
