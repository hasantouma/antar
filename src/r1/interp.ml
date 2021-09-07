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

let optimize expr = expr
