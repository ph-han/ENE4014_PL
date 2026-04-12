let rec deduplicate li = 
  let rec del x li = 
    match li with
      [] -> []
    | hd::tl -> 
      if hd <> x then
        hd :: (del x tl)
      else
        del x tl
  in
  match li with
    [] -> []
  | hd::tl -> hd :: (deduplicate (del hd tl))
;;
