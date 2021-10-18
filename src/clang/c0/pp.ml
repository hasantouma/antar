let pp_a arg : string =
  match arg with
  | Number n -> string_of_int n
  | Var v -> v

let pp_e exp : string =
  match exp with
  | Arg a -> pp_a a
  | Read -> "(read)"
  | Negate a -> "(- " ^ pp_a a ^ ")"
  | Add (l, r) -> "(+ " ^ pp_a l ^ " " ^ pp_a r ^ ")"

let pp_s indent stmt : string =
  let spaces = String.make indent ' ' in
  match stmt with
  | Set (v, e) -> spaces ^ "(set! " ^ v ^ " " ^ pp_e e ^ ")\n"

let rec pp_t indent tail : string =
  let spaces = String.make indent ' ' in
  match tail with
  | Return a -> spaces ^ "(return " ^ pp_a a ^ ")\n"
  | Seq (s, t) -> pp_s indent s ^ pp_t indent t

let pp (p : p) : string =
  let prog = "(program _\n" in
  let indent = 2 in
  let spaces = String.make indent ' ' in
  let spaces' = String.make (indent + 2) ' ' in
  let labels =
    List.fold_left
      (fun acc (label, tail) -> spaces' ^ "[ " ^ label ^ " \n" ^ pp_t (indent + 4) tail ^ spaces' ^ "]\n" ^ acc)
      "" p.blks
  in
  prog ^ spaces ^ "(\n" ^ labels ^ spaces ^ ")\n" ^ ")"
