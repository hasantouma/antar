let interp_open f env read_int expr =
  match expr with
  | `EInt n -> n
  | `ERead -> read_int ()
  | `ENegate e ->
    let e' = f env read_int e in
    let v = -1 * e' in
    v
  | `EAdd (l, r) ->
    let vl = f env read_int l in
    let vr = f env read_int r in
    let v = vl + vr in
    v

let interp ?(env = []) ?input:(read_int = Utils.Repl.make_read []) expr =
  let rec interp env read_int expr = interp_open interp env read_int expr in
  interp env read_int expr

let optimize_open func env expr =
  match expr with
  | `EInt n -> `EInt n
  | `ERead -> `ERead
  | `ENegate e ->
    let rec negate_opt e =
      match e with
      | `EInt n -> `EInt (-1 * n)
      | `ENegate e' -> e'
      | `EAdd (`EInt n, e') -> `EAdd (negate_opt (`EInt n), negate_opt e')
      | e' -> `ENegate e'
    in
    negate_opt (func env e)
  | `EAdd (l, r) -> (
    match (func env l, func env r) with
    | `EInt ln, `EInt rn -> `EInt (ln + rn)
    | `EInt ln, `EAdd (`EInt rn, re) -> `EAdd (`EInt (ln + rn), re)
    | `EAdd (`EInt ln, le), `EInt rn -> `EAdd (`EInt (ln + rn), le)
    | `EAdd (`EInt ln, le), `EAdd (`EInt rn, re) -> `EAdd (`EInt (ln + rn), `EAdd (le, re))
    | le, `EInt rn -> `EAdd (`EInt rn, le)
    | le, re -> `EAdd (le, re))

let optimize ?(env = []) expr =
  let rec optimize env expr = optimize_open optimize env expr in
  optimize env expr
