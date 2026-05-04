(* mapn: (’a -> ’a) -> int -> ’a list -> ’a list *)

let rec iter n f x=
  match n with
  | 0 -> x
  | _ -> iter (n-1) f (f x)
;;

let rec mapn f n l =
  match l with
  | [] -> []
  | hd :: tl ->
      (iter n f) hd :: mapn f n tl
;;
