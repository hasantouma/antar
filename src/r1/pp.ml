
let pp_open f expr indent =
  match expr with
  | `EMult(l, r) ->
      let left = f l (indent + 3) in
      let spaces = String.make (indent + 3) ' ' in
      let right = f r (indent + 3) in
      "(* " ^ left ^ "\n" ^ spaces ^ right ^ ")"
  | #R0.Ast.r0_open as e -> R0.Pp.pp_open f e indent

let rec pp expr indent = pp_open pp expr indent

