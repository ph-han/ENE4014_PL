type btree = Leaf | Node of int * btree * btree

let rec insert_btree x bt = 
  match bt with
    Leaf -> Node(x, Leaf, Leaf)
  | Node(n, l_bt, r_bt) ->
    if n >= x then
      Node(n, (insert_btree x l_bt), r_bt)
    else
      Node(n, l_bt, (insert_btree x r_bt))
;;
