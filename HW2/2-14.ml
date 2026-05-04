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
      else if n = 2 then TIMES [CONST n; VAR var]
      else TIMES [CONST n; POWER (x, n - 1)]
  | SUM e ->
      if e = [] then CONST 0
      else SUM (diff_sum e var)
  | TIMES e ->
      if e = [] then CONST 0
      else let cst = ref 0 in diff_times e var cst
and diff_sum e var =
  match e with
  | [] -> []
  | hd :: tl -> 
    let res = diff (hd, var) in
    if res = CONST 0 then
      diff_sum tl var
    else
      res :: diff_sum tl var
and diff_times e var cst =
  match e with
  | [] -> CONST 0
  | hd :: tl ->
      match hd with
        CONST n -> cst := n; diff_times tl var cst
      | VAR x -> if x = var then CONST (!cst) else CONST 0
      | POWER (x, n) ->
        if x <> var then CONST 0
        else if n = 2 then TIMES [CONST (!cst * n); VAR var]
        else TIMES [CONST (!cst * n); POWER (x, n - 1)] 
      | _ -> diff_times tl var cst 
;;
