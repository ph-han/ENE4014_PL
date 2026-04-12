let rec duplicate li = 
  match li with
    [] -> []
  | hd::tl -> hd :: hd :: duplicate tl
;;
