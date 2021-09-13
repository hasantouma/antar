let pp_open f expr indent =
  match expr with
  | `EInt n -> string_of_int n
  | `ERead -> "(read)"
  | `ENegate e -> "(- " ^ f e indent ^ ")"
  | `EAdd (l, r) ->
    let indent' = indent + 3 in
    let left = f l indent' in
    let spaces = String.make indent' ' ' in
    let right = f r indent' in
    "(+ " ^ left ^ "\n" ^ spaces ^ right ^ ")"

let pp expr =
  let rec pp expr indent = pp_open pp expr indent in
  pp expr 0
