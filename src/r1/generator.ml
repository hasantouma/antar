open Utils.Generator

let num_of_reads_open f expr =
  match expr with
  | `EMult(l, r) -> (f l) + (f r)
  | #R0.Ast.expr_open as e -> R0.Generator.num_of_reads_open f e

let rec num_of_reads expr = num_of_reads_open num_of_reads expr

let rec randp n =
  if n = 0 then
    let random_int = next_int () in
    match next_float ()  with
    | f when f < 0.5 -> `ERead
    | _ -> `EInt random_int
  else
    match next_float () with
    | f when f < 0.5 -> `ENegate (randp (n - 1))
    | _ ->
        let left = randp (n - 1) in
        let right = randp (n - 1) in
        `EAdd (left, right)

let generate_input_for_randp expr : int list =
  let reads : int = num_of_reads expr in
  generate_input reads

