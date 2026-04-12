let rec lany li f =
  match li with
    [] -> false
  | hd::tl -> f hd || lany tl f
;;
