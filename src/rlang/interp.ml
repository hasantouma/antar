open Ast

let interp ?(env = []) ?(inputs = []) (rprog : rprogram) : int =
  let read_int : unit -> int = Utils.Repl.make_read inputs in
  let rec interp env read_int expr =
    match expr with
    | EInt n -> n
    | ERead -> read_int ()
    | ENegate e ->
      let e' = interp env read_int e in
      let v = -1 * e' in
      v
    | EAdd (l, r) ->
      let vl = interp env read_int l in
      let vr = interp env read_int r in
      let v = vl + vr in
      v
    | EVar v -> List.assoc v env
    | ELet (x, ex, eb) ->
      let vx = interp env read_int ex in
      let env' = (x, vx) :: env in
      interp env' read_int eb
  in
  interp env read_int rprog.e
