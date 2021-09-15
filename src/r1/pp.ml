let pp_open f expr indent =
  match expr with
  | `EVar v -> v
  | `ELet (v, ve, be) ->
    let indent' = indent + 5 in
    let variable_expr = f ve indent' in
    let body_expr = f be indent' in
    let spaces = String.make indent' ' ' in
    "(let ([" ^ v ^ " " ^ variable_expr ^ "])\n" ^ spaces ^ body_expr ^ ")"
  | #R0.Ast.expr_open as e -> R0.Pp.pp_open f e indent

let pp expr =
  let rec pp expr indent = pp_open pp expr indent in
  pp expr 0
