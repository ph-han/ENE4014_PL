type nat = O | S of nat ;;

let rec is_even a = 
  match a with
    O -> true
  | S O -> false
  | S (S b') -> is_even b'
;;

let rec leq a b =
  match a, b with
   O, b' -> true
  | a', O -> false
  | (S a'), (S b') -> leq a' b'
;;

(* Example *)
let two = S (S O) ;;
let three = S (S (S O)) ;;

is_even two ;;
is_even three ;;

leq two three;;
leq three two ;;