exception Problem

let rec npower x n= 
  if n < 0 then
    raise Problem
  else
    match n with
      0 -> 1.0
    | _ -> (1.0 /. (float_of_int x)) *. (npower x (n-1))
;;

(* example *)
npower 2 0 ;; (* 1. *)
npower 2 2 ;; (* 0.25 *)
npower 2 3 ;; (*  0.125 *)
npower 2 (-1) ;; (* Exception: Problem. *)
