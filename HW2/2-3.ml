(* alterSum: int list -> int *)

let alterSum l =
  let rec switchop op li =
  match li with
  | [] -> 0
  | hd :: tl -> 
    if op = '+' then hd + switchop '-' tl
    else -hd + switchop '+' tl
  in
  match l with
  | [] -> 0
  | hd :: tl -> 
    hd + switchop '+' tl
;;
