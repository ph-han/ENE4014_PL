let rec lall li f =
  match li with
    [] -> true
  | hd::tl -> f hd && lall tl f
;;
