let pp_open f indent expr =
  match expr with
  | `EVar v -> v
  | `ELet (x, ex, eb) ->
    let indent' = indent + 5 in
    let variable_expr = f indent' ex in
    let body_expr = f indent' eb in
    let spaces = String.make indent' ' ' in
    "(let ([" ^ x ^ " " ^ variable_expr ^ "])\n" ^ spaces ^ body_expr ^ ")"
  | #R0.Ast.expr_open as expr -> R0.Pp.pp_open f indent expr

let pp expr =
  let rec pp indent expr = pp_open pp indent expr in
  pp 0 expr
