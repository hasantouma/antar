open OUnit2

type rtest =
  { expr : string
  ; value : int
  ; inputs : int list
  ; message : string
  }

let make_rprog (e : string) : Rlang.rprogram = e |> Lexing.from_string |> Rlang.parse

(* *** Testing interp *** *)

let test_interp (t : rtest) =
  let rprog = make_rprog t.expr in
  assert_equal t.value (Rlang.interp ~inputs:t.inputs rprog) ~msg:("interp: " ^ t.message) ~printer:string_of_int;
  assert_equal t.value
    (Xlang.interp ~inputs:t.inputs (Passes.passes rprog))
    ~msg:("composition of passes: " ^ t.message) ~printer:string_of_int

(* *** Testing composition of passes *** *)

let test_compiler (t : rtest) =
  let rprog = make_rprog t.expr in
  let inputs = List.map string_of_int t.inputs in
  assert_equal (string_of_int t.value)
    (Xlang.assemble ~inputs ~run:true (Passes.passes rprog))
    ~msg:("composition of passes: " ^ t.message)
    ~printer:(fun x -> x)
