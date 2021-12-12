let () = Random.init (int_of_float (Unix.time ()))
let next_float () : float = Random.float 1.0
let next_int ?(bound = 1024) : unit -> int = fun () -> Random.int bound
let depth () : int = Random.int 10

let generate_input (n : int) : int list =
  let rec aux n acc =
    match n with
    | 0 -> acc
    | _ ->
      let random_int = next_int () in
      aux (n - 1) (random_int :: acc)
  in
  aux n []
