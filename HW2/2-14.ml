type ae = CONST of int
| VAR of string
| POWER of string * int
| TIMES of ae list
| SUM of ae list
;;


(* diff : ae ∗ string → ae *)
let rec diff (exp, var) = 
  match exp with
  | CONST _ -> CONST 0
  | VAR x ->
      if x = var then CONST 1
      else CONST 0
  | POWER (x, n) ->
    if x <> var then CONST 0
    else if n = 0 then CONST 0
    else if n = 1 then CONST 1
    else TIMES [CONST n; POWER (x, n - 1)]
  | SUM e ->
      SUM (diff_sum e var)
  | TIMES e ->
      SUM (diff_times e var)
and diff_sum e var =
  match e with
  | [] -> []
  | hd :: tl -> diff (hd, var) :: diff_sum tl var
and diff_times e var =
  match e with
  | [] -> []
  | hd :: tl ->
      TIMES (diff (hd, var) :: tl) :: add_front hd (diff_times tl var)
and add_front first e =
  match e with
  | [] -> []
  | TIMES li :: tl -> TIMES (first :: li) :: add_front first tl
  | hd :: tl -> TIMES [first; hd] :: add_front first tl
;;
