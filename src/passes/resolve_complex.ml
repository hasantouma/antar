open R1.Ast

let atom (expr : expr) : bool =
  match expr with
  | `EInt _
  | `EVar _ ->
    true
  | _ -> false

let rec resolve_complex (expr : expr) : expr =
  match expr with
  | `EInt _ -> expr
  | `EVar _ -> expr
  | `ERead -> expr
  | `ENegate e when atom e -> expr
  | `ENegate e ->
    let x' = Utils.Fresh.fresh_var () in
    let e' = resolve_complex e in
    `ELet (x', e', `ENegate (`EVar x'))
  | `EAdd (l, r) when atom l && atom r -> expr
  | `EAdd (l, r) when atom l ->
    let x' = Utils.Fresh.fresh_var () in
    let r' = resolve_complex r in
    `ELet (x', r', `EAdd (l, `EVar x'))
  | `EAdd (l, r) when atom r ->
    let x' = Utils.Fresh.fresh_var () in
    let l' = resolve_complex l in
    `ELet (x', l', `EAdd (`EVar x', r))
  | `EAdd (l, r) ->
    let x' = Utils.Fresh.fresh_var () in
    let y' = Utils.Fresh.fresh_var () in
    let l' = resolve_complex l in
    let r' = resolve_complex r in
    `ELet (x', l', `ELet (y', r', `EAdd (`EVar x', `EVar y')))
  | `ELet (x, xe, be) ->
    let xe' = resolve_complex xe in
    let be' = resolve_complex be in
    `ELet (x, xe', be')
