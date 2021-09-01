
let pp_open f expr indent =
  match expr with
  | `EInt n -> string_of_int n
  | `ERead -> "(read)"
  | `ENegate e -> "(- " ^ (f e indent) ^ ")"
  | `EAdd (l, r) ->
      let left = f l (indent + 3) in
      let spaces = String.make (indent + 3) ' ' in
      let right = f r (indent + 3) in
      "(+ " ^ left ^ "\n" ^ spaces ^ right ^ ")"
  | `EMult (l, r) ->
    let left = f l (indent + 3) in
    let spaces = String.make (indent + 3) ' ' in
    let right = f r (indent + 3) in
    "(* " ^ left ^ "\n" ^ spaces ^ right ^ ")"

let rec pp expr indent = pp_open pp expr indent

