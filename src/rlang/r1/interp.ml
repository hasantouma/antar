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
  | `EVar v -> List.assoc v env
  | `ELet (x, ex, eb) ->
    let vx = f env read_int ex in
    let env' = (x, vx) :: env in
    f env' read_int eb

let interp ?(env = []) ?(inputs = []) expr =
  let read_int : unit -> int = Utils.Repl.make_read inputs in
  let rec interp env read_int expr = interp_open interp env read_int expr in
  interp env read_int expr

let simple expr : bool =
  match expr with
  | `EInt _ | `EVar _ -> true
  | _ -> false

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
  | `EVar v -> List.assoc v env
  | `ELet (x, ex, eb) ->
    let ex' = func env ex in
    if simple ex'
    then
      let env' = (x, ex') :: env in
      func env' eb
    else
      let env' = (x, `EVar x) :: env in
      let eb' = func env' eb in
      `ELet (x, ex', eb')

let optimize ?(env = []) expr : Ast.expr =
  let rec optimize env expr = optimize_open optimize env expr in
  optimize env expr
