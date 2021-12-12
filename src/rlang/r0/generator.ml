open Utils.Generator

let rec exp2 (n : int) : Ast.expr =
  match n with
  | 0 -> `EInt 1
  | n ->
    let result = exp2 (n - 1) in
    `EAdd (result, result)

let num_of_reads_open f expr =
  match expr with
  | `EInt _ -> 0
  | `ERead -> 1
  | `ENegate e -> f e
  | `EAdd (l, r) -> f l + f r

let rec num_of_reads expr = num_of_reads_open num_of_reads expr

let randp_open func vars n =
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

let randp ?(vars = []) n =
  let rec randp vars n = randp_open randp vars n in
  randp vars n

let generate_input_for_randp expr : int list =
  let reads : int = num_of_reads expr in
  generate_input reads
