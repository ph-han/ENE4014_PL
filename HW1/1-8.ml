let min l =
  let rec find a li =
      match li with
        [] -> a
      | hd::tl -> 
        if hd < a then
          find hd tl
        else
          find a tl
  in
  match l with
    [] -> 0
  | [a] -> a
  | hd::tl -> find hd tl
;;
