open C0.Ast
open R1.Ast

let exp_to_arg (exp : exp) : arg =
  match exp with
  | Arg x -> x
  | _ -> raise (Failure "explicate_control: Invalid exp to arg")

let rec rlang_to_clang (expr : expr) : exp =
  match expr with
  | EInt n -> Arg (Number n)
  | EVar x -> Arg (Var x)
  | ERead -> Read
  | ENegate e ->
    let e' = rlang_to_clang e in
    let e'' = exp_to_arg e' in
    Negate e''
  | EAdd (l, r) ->
    let l' = rlang_to_clang l in
    let r' = rlang_to_clang r in
    let l'' = exp_to_arg l' in
    let r'' = exp_to_arg r' in
    Add (l'', r'')
  | _ -> raise (Failure "explicate_control: Invalid rlang to clang")

let rec lift (expr : expr) : (var * expr) list * expr =
  match expr with
  | EInt n -> ([], EInt n)
  | EVar x -> ([], EVar x)
  | ERead -> ([], ERead)
  | ENegate e -> ([], ENegate e)
  | EAdd (l, r) -> ([], EAdd (l, r))
  | ELet (x, ex, eb) ->
    let xs, ex' = lift ex in
    let bs, eb' = lift eb in
    (xs @ [ (x, ex') ] @ bs, eb')

let wrap_ret_arg (expr : expr) : tail =
  match expr with
  | EInt n -> Return (Number n)
  | EVar x -> Return (Var x)
  | _ ->
    let exp = rlang_to_clang expr in
    let x = Utils.Fresh.fresh_var () in
    Seq (Set (x, exp), Return (Var x))

let make_clang ((lst, expr) : (var * expr) list * expr) : (label * tail) list =
  let ret_arg = wrap_ret_arg expr in
  let lst' = List.map (fun (v, expr) -> (v, rlang_to_clang expr)) lst in
  let tail = List.fold_right (fun (v, exp) acc -> Seq (Set (v, exp), acc)) lst' ret_arg in
  [ ("entry", tail) ]

let explicate_control (expr : expr) : cprogram =
  let blocks = lift expr |> make_clang in
  { info = []; blks = blocks }
