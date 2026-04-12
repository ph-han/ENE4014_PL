let rec cartesian a b =
  let rec sub_cartesian a b = 
    match a with
      [] -> []
    | a_hd::a_tl -> (a_hd, b) :: sub_cartesian a_tl b
    in
  match b with
    [] -> []
  | b_hd::b_tl ->
    sub_cartesian a b_hd :: cartesian a b_tl
;;
