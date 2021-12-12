let interp_open f env read_int expr =
  match expr with
  | `EVar v -> List.assoc v env
  | `ELet (x, ex, eb) ->
    let vx = f env read_int ex in
    let env' = (x, vx) :: env in
    f env' read_int eb
  | #R0.Ast.expr_open as expr -> R0.Interp.interp_open f env read_int expr

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
  | #R0.Ast.expr_open as expr -> R0.Interp.optimize_open func env expr

let optimize ?(env = []) expr : Ast.expr =
  let rec optimize env expr = optimize_open optimize env expr in
  optimize env expr
