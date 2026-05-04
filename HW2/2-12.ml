type talkingto = (string * string) list ;;
let party = [("A", "B"); ("B", "A"); ("A", "D"); ("B", "C"); ("C", "E")] ;;

(* infected : talkingto -> string -> string -> bool *)

let rec infected info p1 p2 =
  match info with
   [] -> false
  | hd::tl ->
    let n1,n2 = hd in
    if n1 = p1 then (n2 = p2) || (infected tl n2 p2) || (infected tl n1 p2)
    else infected tl p1 p2
;;
