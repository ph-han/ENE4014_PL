type nat = O | S of nat ;;

let rec natadd a b = 
  match b with
    O -> a
  | (S b') -> natadd (S (a)) b'
;;

let rec natmul a b = 
  match b with
    O -> O
  | (S b') -> natadd a (natmul a b')
;;

(* Example *)
let two = S (S O) ;;
let three = S (S (S O)) ;;

natadd two three ;;
natmul two three;;