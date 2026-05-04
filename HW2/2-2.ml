(* union: ’a list -> ’a list -> ’a list *)

let rec union l1 l2 =
  let rec isin x l =
    match l with
    | [] -> false
    | hd :: tl -> if x = hd then true else isin x tl
  in
  match l1 with
  | [] -> l2
  | hd :: tl ->
    if isin hd l2 then union tl l2
    else hd :: union tl l2
;;
