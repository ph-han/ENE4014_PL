let rec replicate li n = 
  let rec cat x n l = 
    match n with
      0 -> l
    | _ -> x :: cat x (n-1) l
  in
  match li with
    [] -> []
  | hd::tl -> cat hd n (replicate tl n)
;;
