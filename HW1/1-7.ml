exception Problem ;;

let rec gcd n m =
  if n < m then
    raise Problem
  else
    match m with
      0 -> n
    | _ -> gcd m (n mod m)
;;
