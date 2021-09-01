
let interp_open f expr read_int =
  match expr with
  | `EMult(l, r) ->
      let vl = f l read_int in
      let vr = f r read_int in
      let v = vl * vr in
      v
  | #R0.Ast.r0_open as e -> R0.Interp.interp_open f e read_int

let rec interp expr read_int = interp_open interp expr read_int

