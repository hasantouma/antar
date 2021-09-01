
let interp_open f expr read_int =
  match expr with
  | `EInt n -> n
  | `ERead -> read_int ()
  | `ENegate e ->
      let e' = f e read_int in
      let v = (-1) * e' in
      v
  | `EAdd(l, r) ->
      let vl = f l read_int in
      let vr = f r read_int in
      let v = vl + vr in
      v

let rec interp expr read_int = interp_open interp expr read_int

