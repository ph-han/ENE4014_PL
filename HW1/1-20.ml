let rec powerset li =
  let rec add_subset x li = 
    match li with
    | [] -> []
    | hd :: tl -> (x :: hd) :: add_subset x tl
  in
  let rec append l1 l2 =
    match l1 with
    | [] -> l2
    | hd :: tl -> hd :: append tl l2
  in
  match li with
    [] -> [[]]
  | hd :: tl -> 
    append (powerset tl) (add_subset hd (powerset tl))
;;
