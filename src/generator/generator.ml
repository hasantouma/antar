open Lex_parse.Ast

let () = Random.init (int_of_float (Unix.time ()))
let random_float_gen : (float -> float) = Random.float
let random_int_gen : (int -> int) = Random.int

let rec exp2 (n : int) : expr =
  match n with
  | 0 -> `EInt 1
  | n ->
      let left = exp2 (n - 1) in
      let right = exp2 (n - 1) in
      `EAdd(left, right)

let rec randp (n : int) : expr =
  let random_float = random_float_gen 1.0 in
  if n = 0 then
    let random_int = random_int_gen 1024 in
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


