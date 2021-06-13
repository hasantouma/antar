open Ast
open Utils.Generator

let rec exp2 (n : int) : expr =
  match n with
  | 0 -> `EInt 1
  | n ->
      let result = exp2 (n - 1) in
      `EAdd(result, result)

let num_of_reads (expr : expr) : int =
  let rec aux expr acc =
    match expr with
    | `EInt _ -> acc
    | `ERead -> 1 + acc
    | `EAdd(l, r) -> (aux l acc) + (aux r acc)
    | `ENegate e -> aux e acc
  in aux expr 0

let rec randp (n : int) : expr =
  let random_float = next_float () in
  if n = 0 then
    let random_int = next_int () in
    match random_float with
    | f when f < 0.5 -> `ERead
    | _ -> `EInt random_int
  else
    match random_float with
    | f when f < 0.5 -> `ENegate (randp (n - 1))
    | _ ->
        let left = randp (n - 1) in
        let right = randp (n - 1) in
        `EAdd (left, right)

let generate_input_for_randp (expr : expr) : int list =
  let reads : int = num_of_reads expr in
  let rec aux n acc =
    match n with
    | 0 -> acc
    | _ ->
        let random_int = next_int () in
        aux (n - 1) (random_int :: acc)
  in
  aux reads []

