
let pp_open f expr =
  match expr with
  | `EInt n -> string_of_int n
  | `ERead -> "(read)"
  | `ENegate e -> "(- " ^ (f e) ^ ")"
  | `EAdd(l, r) -> "(+ " ^ (f l) ^ " " ^ (f r) ^ ")"

let rec pp expr = pp_open pp expr
