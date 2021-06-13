
let () = Random.init (int_of_float (Unix.time ()))

let random_float_gen : (float -> float) = Random.float
let next_float () : float = random_float_gen 1.0

let random_int_gen : (int -> int) = Random.int
let next_int () : int = random_int_gen 1024

