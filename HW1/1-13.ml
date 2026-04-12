type btree = Leaf | Node of int * btree * btree

let rec double_tree bt = 
  match bt with
    Leaf -> Leaf
  | Node(n, Leaf, Leaf) ->
      Node(n * 2, Leaf, Leaf)
  | Node(n, l_bt, r_bt) ->
    Node(n, double_tree l_bt, double_tree r_bt)
;;
