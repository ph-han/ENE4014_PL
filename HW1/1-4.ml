type nat = O | S of nat
type nat_list = Nil | Cons of nat * nat_list

let rec is_even a = 
  match a with
    O -> true
  | S O -> false
  | S (S a') -> is_even a'
;;

let rec for_all f l =
  match l with
    Nil -> true
  | Cons(hd, tl) -> (f hd) && (for_all f tl)
;;
