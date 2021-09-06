let pp_open f expr indent =
  match expr with
  | `EVar v -> v
  | `ELet (v, ve, be) ->
    let variable_expr = f ve indent in
    let body_expr = f be indent in
    "(let ([" ^ v ^ " " ^ variable_expr ^ "]) " ^ body_expr ^ ")"
  | #R0.Ast.r0_open as e -> R0.Pp.pp_open f e indent

let pp expr =
  let rec pp expr indent = pp_open pp expr indent in
  pp expr 0
