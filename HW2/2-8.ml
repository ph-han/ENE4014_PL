type 'a ntree =
  | Leaf of 'a
  | Node of 'a ntree list

(* findn: ’a ntree -> int *)

let rec findn t =
  let max a b = if a > b then a else b in
  let rec len li =
    match li with
      [] -> 0
    | nd::tl -> 1 + len tl
  in
  let rec iter_node node =
    match node with
        [] -> 0
      | hd :: tl -> 
        max (findn hd) (iter_node tl)
  in
  match t with
    Leaf _ -> 0
  | Node sub -> max (len sub) (iter_node sub)
;;

