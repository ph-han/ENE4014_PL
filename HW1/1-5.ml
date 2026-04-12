type nat = O | S of nat
type nat_list = Nil | Cons of nat * nat_list

let rec filter_not f l =
  match l with
    Nil -> l
  | Cons(hd, tl) -> 
      if (f hd) = false then 
        Cons(hd, (filter_not f tl))
      else
        filter_not f tl
;;
