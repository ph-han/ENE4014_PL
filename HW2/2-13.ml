type talkingto = (string * string) list ;;
let party = [("A", "B"); ("B", "A"); ("A", "D"); ("B", "C"); ("C", "E")] ;;

(* infected_vaccine : talkingto -> string list -> string -> string -> bool *)

let rec infected_vaccine info vaccine p1 p2 =
  let rec check_not_vaccine vac p = 
    match vac with
      [] -> true
    | hd::tl -> 
      if hd = p then  false
      else check_not_vaccine tl p
  in
  match info with
   [] -> false
  | hd::tl ->
    let n1,n2 = hd in
    if (check_not_vaccine vaccine p1) && n1 = p1 then 
      (n2 = p2) || 
      (infected_vaccine tl vaccine n2 p2) || 
      (infected_vaccine tl vaccine n1 p2)
    else infected_vaccine tl vaccine p1 p2
;;
