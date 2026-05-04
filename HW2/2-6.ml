(* sigma : int * int * (int -> int) -> int *)

let sigma (a, b, f) =
  let rec plus i =
    if i > b then 0
    else f i + plus (i + 1)
  in
  plus a
;;
