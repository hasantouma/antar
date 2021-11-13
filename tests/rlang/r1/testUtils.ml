open OUnit2
open R1.Interp

type rtest =
  { expr : string
  ; optimized : string
  ; value : int
  ; inputs : int list
  ; message : string
  }

let make_prog (e : string) : R1.Ast.program = e |> Lexing.from_string |> R1.Lang.parse

(* *** Testing without optimize pass *** *)

let test_interp (t : rtest) =
  let p = make_prog t.expr in
  let pass_values = R_test_utils.r1_to_passes p.e in
  assert_equal t.value (interp ~inputs:t.inputs p.e) ~msg:("interp: " ^ t.message) ~printer:string_of_int;
  assert_equal t.value
    (interp ~inputs:t.inputs pass_values.uniquify)
    ~msg:("r1_to_uniquify: " ^ t.message) ~printer:string_of_int;
  assert_equal true pass_values.is_uniquify ~msg:("is_uniquify: " ^ t.message) ~printer:string_of_bool;
  assert_equal t.value
    (interp ~inputs:t.inputs pass_values.resolve_complex)
    ~msg:("r1_to_resolve_complex: " ^ t.message) ~printer:string_of_int;
  assert_equal true pass_values.is_resolve_complex ~msg:("is_resolve_complex: " ^ t.message) ~printer:string_of_bool;
  assert_equal t.value
    (C0.Interp.interp ~inputs:t.inputs pass_values.explicate_control)
    ~msg:("r1_to_explicate_control: " ^ t.message)
    ~printer:string_of_int

(* *** Testing with optimize pass *** *)

let test_optimize (t : rtest) =
  let p_expr = make_prog t.expr in
  let pass_values = R_test_utils.r1_to_passes p_expr.e in
  let p_opt = make_prog t.optimized in
  let opt_value = interp ~inputs:t.inputs p_opt.e in
  assert_equal opt_value (interp ~inputs:t.inputs p_expr.e)
    ~msg:("optimize vs. non-optimize: " ^ t.message)
    ~printer:string_of_int;
  assert_equal p_opt.e pass_values.optimize ~msg:("optimize AST vs. expr optimize AST: " ^ t.message) ~printer:R1.Pp.pp;
  assert_equal opt_value
    (interp ~inputs:t.inputs pass_values.opt_uniquify)
    ~msg:("opt_uniquify: " ^ t.message) ~printer:string_of_int;
  assert_equal true pass_values.opt_is_uniquify ~msg:("opt_is_uniquify: " ^ t.message) ~printer:string_of_bool;
  assert_equal opt_value
    (interp ~inputs:t.inputs pass_values.opt_resolve_complex)
    ~msg:("opt_resolve_complex: " ^ t.message) ~printer:string_of_int;
  assert_equal true pass_values.opt_is_resolve_complex
    ~msg:("opt_is_resolve_complex: " ^ t.message)
    ~printer:string_of_bool;
  assert_equal opt_value
    (C0.Interp.interp ~inputs:t.inputs pass_values.opt_explicate_control)
    ~msg:("opt_explicate_control: " ^ t.message) ~printer:string_of_int
