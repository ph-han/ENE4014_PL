type btree = Leaf | Node of int * btree * btree

let rec count_oddnode bt = 
  match bt with
    Leaf ->  0
  | Node(n, bt1, bt2) -> 
    if (n mod 2) = 1 then
      1 + count_oddnode bt1 + count_oddnode bt2
    else
      count_oddnode bt1 + count_oddnode bt2
;;
