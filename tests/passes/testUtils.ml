let rlang_alpha_equiv e1 e2 : bool =
  let rec go env e1 e2 =
    match (e1, e2) with
    | `EInt n, `EInt m -> n = m
    | `ERead, `ERead -> true
    | `EVar x, `EVar y -> (
      let y_opt = List.assoc_opt x env in
      match y_opt with
      | Some y' -> y = y'
      | None -> x = y)
    | `ENegate e1', `ENegate e2' -> go env e1' e2'
    | `EAdd (l1, r1), `EAdd (l2, r2) -> go env l1 l2 && go env r1 r2
    | `ELet (x, xe, be1), `ELet (y, ye, be2) ->
      let env' = (x, y) :: env in
      go env xe ye && go env' be1 be2
    | _ -> false
  in
  go [] e1 e2
