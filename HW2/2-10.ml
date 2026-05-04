type formula = TRUE | FALSE
| NOT of formula
| ANDALSO of formula * formula
| ORELSE of formula * formula
| IMPLY of formula * formula
| LESS of expr * expr
and expr = NUM of int
| PLUS of expr * expr
| MINUS of expr * expr
;;

(* eval : formula → bool *)

let rec eval exp =
  match exp with
    TRUE -> true
  | FALSE -> false
  | NOT e -> let v = eval e in not v
  | ANDALSO (e1, e2) -> eval e1 && eval e2
  | ORELSE (e1, e2) -> eval e1 || eval e2
  | IMPLY (e1, e2) -> eval (NOT e1) || eval e2
  | LESS (e1, e2) -> eval_expr e1 < eval_expr e2
and eval_expr e =
  match e with
    NUM n -> n
  | PLUS (e1, e2) -> eval_expr e1 + eval_expr e2
  | MINUS (e1, e2) -> eval_expr e1 - eval_expr e2
;;

