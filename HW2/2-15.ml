type exp = X | INT of int
| REAL of float
| ADD of exp * exp
| SUB of exp * exp
| MUL of exp * exp
| DIV of exp * exp
| SIGMA of exp * exp * exp
| INTEGRAL of exp * exp * exp
;;

(* calculate : exp → float *)

let calculate expr = 
  let rec eval e x = 
    match e with
      X -> x
    | INT n -> float_of_int n
    | REAL f -> f
    | ADD (e1, e2) -> eval e1 x +. eval e2 x
    | SUB (e1, e2) -> eval e1 x -. eval e2 x
    | MUL (e1, e2) -> eval e1 x *. eval e2 x
    | DIV (e1, e2) -> eval e1 x /. eval e2 x
    | SIGMA (e1, e2, e3) -> 
        let st = eval e1 x in
        let ed = eval e2 x in
        let rec sigma i res =
          if i > ed then res
          else sigma (i +. 1.0) (res +. (eval e3 i))
        in
          sigma st 0.0
    | INTEGRAL (e1, e2, e3) -> 
        let dx = 0.1 in
        let st = eval e1 x in
        let ed = eval e2 x in
        let rec integral i res =
          if i > ed then res
          else integral (i +. dx) (res +. (eval e3 i) *. dx)
        in
          integral st 0.0
    in
      eval expr 0.0
  ;;
