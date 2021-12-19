let pp_open f indent expr =
  match expr with
  | `EInt n -> string_of_int n
  | `ERead -> "(read)"
  | `ENegate e -> "(- " ^ f indent e ^ ")"
  | `EAdd (l, r) ->
    let indent' = indent + 3 in
    let left = f indent' l in
    let spaces = String.make indent' ' ' in
    let right = f indent' r in
    "(+ " ^ left ^ "\n" ^ spaces ^ right ^ ")"
  | `EVar v -> v
  | `ELet (x, ex, eb) ->
    let indent' = indent + 5 in
    let variable_expr = f indent' ex in
    let body_expr = f indent' eb in
    let spaces = String.make indent' ' ' in
    "(let ([" ^ x ^ " " ^ variable_expr ^ "])\n" ^ spaces ^ body_expr ^ ")"

let pp expr =
  let rec pp indent expr = pp_open pp indent expr in
  pp 0 expr
