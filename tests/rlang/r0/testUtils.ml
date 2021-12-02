open OUnit2
open R0.Interp

type rtest =
  { expr : string
  ; optimized : string
  ; value : int
  ; inputs : int list
  ; message : string
  }

let make_rprog (e : string) : R0.Lang.rprogram = e |> Lexing.from_string |> R0.Lang.parse

let test_interp (t : rtest) =
  let p = make_rprog t.expr in
  assert_equal t.value (interp ~inputs:t.inputs p.e) ~msg:("interp: " ^ t.message) ~printer:string_of_int

let test_optimize (t : rtest) =
  let p_expr = make_rprog t.expr in
  let p_opt = make_rprog t.optimized in
  assert_equal (interp ~inputs:t.inputs p_opt.e) (interp ~inputs:t.inputs p_expr.e)
    ~msg:("optimize vs. non-optimize: " ^ t.message)
    ~printer:string_of_int;
  assert_equal p_opt.e (optimize p_expr.e) ~msg:("opt-optimize vs. optimize: " ^ t.message) ~printer:R0.Pp.pp
