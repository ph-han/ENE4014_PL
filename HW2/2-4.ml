(* dsort: int list -> int list *)

let rec dsort l =
  let rec insert x li =
    match li with
    | [] -> [x]
    | hd :: tl -> 
      if x - hd >= 0 then 
        x :: li
      else
        hd :: insert x tl
  in
  match l with
  | [] -> []
  | hd :: tl -> insert hd (dsort tl)
;;
