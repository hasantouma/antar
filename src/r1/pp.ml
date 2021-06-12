
let pp_open f expr =
  match expr with
  | `EMult(l, r) -> "(* " ^ (f l) ^ " " ^ (f r) ^ ")"
  | #R0.Ast.expr_open as e -> R0.Pp.pp_open f e

let rec pp expr = pp_open pp expr

