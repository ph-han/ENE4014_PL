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

let rec leq a b =
  match a, b with
   O, b' -> true
  | a', O -> false
  | (S a'), (S b') -> leq a' b'
;;


let is_squared a = 
  let rec check n = 
    if leq n a then 
      if (natmul n n) = a then
        true
      else
        check (S (n))
    else false
  in
  match a with
    O -> false
  | _ -> check (S O)
;;

(* Example *)
let two = S (S O) ;;
let three = S (S (S O)) ;;

is_squared two ;;
let nine = natmul three three ;;
is_squared nine ;;
let four = natmul two two ;;
is_squared four ;;
