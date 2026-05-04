type 'a ntree =
  | Leaf of 'a
  | Node of 'a ntree list

(* flatten: ’a ntree -> ’a list *)
let rec flatten t = 
  let rec iter_node node =
    match node with
        [] -> []
      | hd :: tl -> 
        (flatten hd) @ (iter_node tl)
  in
  match t with
    Leaf a' -> [a']
  | Node sub -> iter_node sub
;;
