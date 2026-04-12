type btree = Leaf | Node of int * btree * btree

let rec count_leaves bt = 
  match bt with
    Leaf ->  1
  | Node(_, bt1, bt2) -> count_leaves bt1 + count_leaves bt2
