(* revrev: ’a list list -> ’a list list *)

let rec revrev l = 
  let rec rev l = 
    match l with
    | [] -> []
    | hd :: tl -> rev tl @ [hd]
  in
  match l with  
  | [] -> []
  | hd :: tl -> revrev tl @ [rev hd]
;;
