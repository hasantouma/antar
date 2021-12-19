open Utils.Generator

let rec exp2 (n : int) : Ast.expr =
  match n with
  | 0 -> `EInt 1
  | n ->
    let result = exp2 (n - 1) in
    `EAdd (result, result)

let rec num_of_reads expr =
  match expr with
  | `EInt _ -> 0
  | `ERead -> 1
  | `ENegate e -> num_of_reads e
  | `EAdd (l, r) -> num_of_reads l + num_of_reads r
  | `EVar _ -> 0
  | `ELet (_, ex, eb) ->
    let vx = num_of_reads ex in
    let vb = num_of_reads eb in
    vx + vb

let base_case func vars n : Ast.expr =
  if n = 0
  then
    let sign = if next_float () < 0.5 then 1 else -1 in
    let random_int = sign * next_int () in
    match next_float () with
    | f when f < 0.5 -> `ERead
    | _ -> `EInt random_int
  else
    match next_float () with
    | f when f < 0.5 -> `ENegate (func vars (n - 1))
    | _ ->
      let left = func vars (n - 1) in
      let right = func vars (n - 1) in
      `EAdd (left, right)

let choice func vars =
  match vars with
  | [] -> base_case func vars 0
  | _ -> (
    match next_float () with
    | f when f < 0.66 -> base_case func vars 0
    | _ ->
      let rand_index = next_int ~bound:(List.length vars) () in
      let v = List.nth vars rand_index in
      `EVar v)

let choose_vars vars =
  if List.length vars > 0 && next_float () < 0.5
  then vars
  else
    let x = Utils.Fresh.fresh_var () in
    x :: vars

let randp ?(vars = []) n =
  let rec randp vars n =
    if n = 0
    then choice randp vars
    else
      match next_float () with
      | f when f < 0.66 -> base_case randp vars n
      | _ ->
        let vars' = choose_vars vars in
        let x = List.hd vars' in
        let ex = randp vars (n - 1) in
        let eb = randp vars' (n - 1) in
        `ELet (x, ex, eb)
  in
  randp vars n

let generate_input_for_randp expr : int list =
  let reads : int = num_of_reads expr in
  generate_input reads
