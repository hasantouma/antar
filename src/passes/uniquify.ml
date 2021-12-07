open R1.Ast

(* helper *)
let rec dup_exist lst =
  match lst with
  | [] -> false
  | hd :: tl -> List.exists (( = ) hd) tl || dup_exist tl

let uniquify (expr : expr) : expr =
  let rec uniquify rho expr =
    match expr with
    | `EInt _
    | `ERead ->
      expr
    | `ENegate e ->
      let e' = uniquify rho e in
      `ENegate e'
    | `EAdd (l, r) ->
      let l' = uniquify rho l in
      let r' = uniquify rho r in
      `EAdd (l', r')
    | `EVar x ->
      let x' = List.assoc x rho in
      `EVar x'
    | `ELet (x, xe, be) ->
      let x' = Utils.Fresh.fresh_var () in
      let xe' = uniquify rho xe in
      let rho' = (x, x') :: rho in
      let be' = uniquify rho' be in
      `ELet (x', xe', be')
  in
  uniquify [] expr

(* is_uniquify *)
let is_uniquify (expr : expr) : bool =
  let rec is_uniquify expr : string list =
    match expr with
    | `EInt _
    | `ERead ->
      []
    | `ENegate e -> is_uniquify e
    | `EAdd (l, r) ->
      let l' = is_uniquify l in
      let r' = is_uniquify r in
      l' @ r'
    | `EVar _ -> [] (* Only variables at declaration should be counted *)
    | `ELet (x, xe, be) ->
      let xe' = is_uniquify xe in
      let be' = is_uniquify be in
      [ x ] @ xe' @ be'
  in
  let var_list = is_uniquify expr in
  var_list |> dup_exist |> not
