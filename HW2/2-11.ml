type circuit = IN
| AND of circuit * circuit
| OR of circuit * circuit
;;

(* and_depth : circuit -> int *)

let rec and_depth c = 
  let max v1 v2 = if v1 < v2 then v2 else v1 in
  match c with
    IN -> 0
  | OR (c1, c2) -> max (and_depth c1) (and_depth c2)
  | AND (c1, c2) -> 1 + max (and_depth c1) (and_depth c2)
;;
