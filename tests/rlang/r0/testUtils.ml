open OUnit2
open R0.Interp
open R_test_utils

let make_prog (e : string) = e |> Lexing.from_string |> R0.Lang.parse

let test_interp (t : rtest) =
  let p = make_prog t.expr in
  assert_equal t.value (interp ~inputs:t.inputs p.e) ~msg:("interp: " ^ t.message) ~printer:string_of_int

let test_optimize (t : rtest) =
  let p_expr = make_prog t.expr in
  let p_opt = make_prog t.optimized in
  assert_equal (interp ~inputs:t.inputs p_opt.e) (interp ~inputs:t.inputs p_expr.e)
    ~msg:("optimize vs. non-optimize: " ^ t.message)
    ~printer:string_of_int;
  assert_equal p_opt.e (optimize p_expr.e) ~msg:("opt-optimize vs. optimize: " ^ t.message) ~printer:R1.Pp.pp
