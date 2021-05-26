
let pp h expr =
  match expr with
  | `EInt n -> string_of_int n
  | `ERead -> "(read)"
  | `ENegate e -> "(- " ^ (h e) ^ ")"
  | `EAdd(l, r) -> "(+ " ^ (h l) ^ " " ^ (h r) ^ ")"

