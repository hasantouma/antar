
let interp_open f expr read_int =
  match expr with
  | `EInt n -> n
  | `ERead -> read_int ()
  | `ENegate e ->
      let e' = f e read_int in
      let v = (-1) * e' in
      v
  | `EAdd (l, r) ->
      let vl = f l read_int in
      let vr = f r read_int in
      let v = vl + vr in
      v
  | `EMult (l, r) ->
    let vl = f l read_int in
    let vr = f r read_int in
    let v = vl * vr in
    v

let rec interp expr read_int = interp_open interp expr read_int

let rec optimize expr =
  match expr with
  | `EInt n -> `EInt n
  | `ERead -> `ERead
  | `ENegate e ->
    let rec negate_opt e =
      (
        match e with
        | `EInt n -> `EInt (-1 * n)
        | `ENegate e' -> e'
        | `EAdd (`EInt n, e') -> `EAdd (negate_opt (`EInt n), negate_opt e')
        | e' -> `ENegate e'
      )
    in
    negate_opt (optimize e)
  | `EAdd (l, r) ->
    match (optimize l, optimize r) with
    | (`EInt ln, `EInt rn) -> `EInt (ln + rn)
    | (`EInt ln, `EAdd (`EInt rn, re)) -> `EAdd (`EInt (ln + rn), re)
    | (`EAdd (`EInt ln, le), `EInt rn) -> `EAdd (`EInt (ln + rn), le)
    | (`EAdd (`EInt ln, le), `EAdd (`EInt rn, re)) -> `EAdd (`EInt (ln + rn), `EAdd (le, re))
    | (le, `EInt rn) -> `EAdd (`EInt rn, le)
    | (le, re) -> `EAdd (le, re)

