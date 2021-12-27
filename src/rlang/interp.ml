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

let simple expr : bool =
  match expr with
  | EInt _ | EVar _ -> true
  | _ -> false

let optimize ?(env = []) (rprog : rprogram) : rprogram =
  let rec optimize env expr =
    match expr with
    | EInt n -> EInt n
    | ERead -> ERead
    | ENegate e ->
      let rec negate_opt e =
        match e with
        | EInt n -> EInt (-1 * n)
        | ENegate e' -> e'
        | EAdd (EInt n, e') -> EAdd (negate_opt (EInt n), negate_opt e')
        | e' -> ENegate e'
      in
      negate_opt (optimize env e)
    | EAdd (l, r) -> (
      match (optimize env l, optimize env r) with
      | EInt ln, EInt rn -> EInt (ln + rn)
      | EInt ln, EAdd (EInt rn, re) -> EAdd (EInt (ln + rn), re)
      | EAdd (EInt ln, le), EInt rn -> EAdd (EInt (ln + rn), le)
      | EAdd (EInt ln, le), EAdd (EInt rn, re) -> EAdd (EInt (ln + rn), EAdd (le, re))
      | le, EInt rn -> EAdd (EInt rn, le)
      | le, re -> EAdd (le, re))
    | EVar v -> List.assoc v env
    | ELet (x, ex, eb) ->
      let ex' = optimize env ex in
      if simple ex'
      then
        let env' = (x, ex') :: env in
        optimize env' eb
      else
        let env' = (x, EVar x) :: env in
        let eb' = optimize env' eb in
        ELet (x, ex', eb')
  in
  let expr' = optimize env rprog.e in
  { rprog with e = expr' }
