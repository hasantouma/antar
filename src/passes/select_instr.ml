open C0.Ast

let var_of_stmt (s : stmt) : var =
  match s with
  | Set (x, _) -> x

let rec get_vars (acc : var list) (tail : tail) : var list =
  match tail with
  | Return _ -> acc
  | Seq (s, t) ->
    let x = var_of_stmt s in
    get_vars (x :: acc) t

let uncover_locals (p : C0.Ast.cprogram) : C0.Ast.cprogram =
  let tail = List.assoc "entry" p.blks in
  let info' = get_vars [] tail in
  { p with info = info' }
