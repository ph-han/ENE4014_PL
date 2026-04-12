type btree = Leaf | Node of int * btree * btree
type path = NoPath | Path of direction list and direction = Left | Right

let rec tree_path bt x = 
  let add_path dir p = 
    match p with
      NoPath -> NoPath
      | Path (li) -> Path (dir :: li)
  in
  match bt with
    Leaf -> NoPath
  | Node(n, l_bt, r_bt) -> 
      if n = x then
        Path []
      else
        let l_path = tree_path l_bt x in
        let r_path = tree_path r_bt x in
          match l_path with
            NoPath -> add_path Right r_path
          | Path(li) -> add_path Left l_path
;;
          
