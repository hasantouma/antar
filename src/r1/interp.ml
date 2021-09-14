let interp_open f expr env read_int =
  match expr with
  | `EVar v -> List.assoc v env
  | `ELet (x, ex, eb) ->
    let vx = f ex env read_int in
    let env' = (x, vx) :: env in
    f eb env' read_int
  | #R0.Ast.r0_open as e -> R0.Interp.interp_open f e env read_int

let interp ?(env = []) ?(input = Utils.Repl.make_read []) expr =
  let rec interp expr env read_int = interp_open interp expr env read_int in
  interp expr env input

let simple expr : bool =
  match expr with
  | `EInt _
  | `EVar _ ->
    true
  | _ -> false

let optimize_open func env expr =
  match expr with
  | `EVar v -> List.assoc v env
  | `ELet (x, ex, eb) ->
    let ex' = func env ex in
    if simple ex' then
      let env' = (x, ex') :: env in
      func env' eb
    else
      let env' = (x, `EVar x) :: env in
      let eb' = func env' eb in
      `ELet (x, ex', eb')
  | #R0.Ast.r0_open as e -> R0.Interp.optimize_open func env e

let optimize ?(env = []) expr : Ast.r1 =
  let rec optimize env expr = optimize_open optimize env expr in
  optimize env expr
