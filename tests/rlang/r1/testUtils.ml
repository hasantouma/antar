open OUnit2
open R1.Interp
open R_test_utils

let make_prog (e : string) = e |> Lexing.from_string |> R1.Lang.parse

(* *** Testing without optimized pass *** *)

(* uniquify pass *)
let r1_to_uniquify (e : R1.Ast.expr) : R1.Ast.expr = e |> Passes.Uniquify.uniquify

let is_uniquify (e : R1.Ast.expr) : bool = e |> r1_to_uniquify |> Passes.Uniquify.is_uniquify

(* resolve_complex pass *)
let r1_to_resolve_complex (e : R1.Ast.expr) : R1.Ast.expr =
  e |> r1_to_uniquify |> Passes.Resolve_complex.resolve_complex

let is_resolve_complex (e : R1.Ast.expr) : bool =
  e |> r1_to_resolve_complex |> Passes.Resolve_complex.is_resolve_complex

(* explicate_control pass *)
let r1_to_explicate_control (e : R1.Ast.expr) : C0.Ast.p =
  e |> r1_to_resolve_complex |> Passes.Explicate_control.explicate_control

let test_interp (t : rtest) =
  let p = make_prog t.expr in
  assert_equal t.value (interp ~inputs:t.inputs p.e) ~msg:("interp: " ^ t.message) ~printer:string_of_int;
  assert_equal t.value
    (interp ~inputs:t.inputs (r1_to_uniquify p.e))
    ~msg:("r1_to_uniquify: " ^ t.message) ~printer:string_of_int;
  assert_equal true (is_uniquify p.e) ~msg:("is_uniquify: " ^ t.message) ~printer:string_of_bool;
  assert_equal t.value
    (interp ~inputs:t.inputs (r1_to_resolve_complex p.e))
    ~msg:("r1_to_resolve_complex: " ^ t.message) ~printer:string_of_int;
  assert_equal true (is_resolve_complex p.e) ~msg:("is_resolve_complex: " ^ t.message) ~printer:string_of_bool;
  assert_equal t.value
    (C0.Interp.interp ~inputs:t.inputs (r1_to_explicate_control p.e))
    ~msg:("r1_to_explicate_control: " ^ t.message)
    ~printer:string_of_int

(* *** Testing with optimized pass *** *)

(* optimized pass *)
let r1_to_optimized (e : R1.Ast.expr) : R1.Ast.expr = e |> optimize

(* uniquify pass *)
let opt_uniquify (e : R1.Ast.expr) : R1.Ast.expr = e |> r1_to_optimized |> Passes.Uniquify.uniquify

let opt_is_uniquify (e : R1.Ast.expr) : bool = e |> opt_uniquify |> Passes.Uniquify.is_uniquify

(* resolve_complex pass *)
let opt_resolve_complex (e : R1.Ast.expr) : R1.Ast.expr = e |> opt_uniquify |> Passes.Resolve_complex.resolve_complex

let opt_is_resolve_complex (e : R1.Ast.expr) : bool =
  e |> opt_resolve_complex |> Passes.Resolve_complex.is_resolve_complex

(* explicate_control pass *)
let opt_explicate_control (e : R1.Ast.expr) : C0.Ast.p =
  e |> opt_resolve_complex |> Passes.Explicate_control.explicate_control

let test_optimize (t : rtest) =
  let p_expr = make_prog t.expr in
  let p_opt = make_prog t.optimized in
  let opt_value = interp ~inputs:t.inputs p_opt.e in
  assert_equal opt_value (interp ~inputs:t.inputs p_expr.e)
    ~msg:("optimize vs. non-optimize: " ^ t.message)
    ~printer:string_of_int;
  assert_equal p_opt.e (r1_to_optimized p_expr.e)
    ~msg:("optimize AST vs. expr optimize AST: " ^ t.message)
    ~printer:R1.Pp.pp;
  assert_equal opt_value
    (interp ~inputs:t.inputs (opt_uniquify p_expr.e))
    ~msg:("opt_uniquify: " ^ t.message) ~printer:string_of_int;
  assert_equal true (opt_is_uniquify p_expr.e) ~msg:("opt_is_uniquify: " ^ t.message) ~printer:string_of_bool;
  assert_equal opt_value
    (interp ~inputs:t.inputs (opt_resolve_complex p_expr.e))
    ~msg:("opt_resolve_complex: " ^ t.message) ~printer:string_of_int;
  assert_equal true (opt_is_resolve_complex p_expr.e)
    ~msg:("opt_is_resolve_complex: " ^ t.message)
    ~printer:string_of_bool;
  assert_equal opt_value
    (C0.Interp.interp ~inputs:t.inputs (opt_explicate_control p_expr.e))
    ~msg:("opt_explicate_control: " ^ t.message) ~printer:string_of_int
