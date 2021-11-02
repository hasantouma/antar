open OUnit2
open Parser.Main
open R1.Interp

type test =
  { expr : string
  ; optimized : string
  ; value : int
  ; inputs : int list
  ; message : string
  }

let test_interp (t : test) =
  let lexbuf = Lexing.from_string t.expr in
  let p = make_prog lexbuf in
  assert_equal t.value (interp ~inputs:t.inputs p.e) ~msg:("interp: " ^ t.message) ~printer:string_of_int;
  assert_equal t.value
    (interp ~inputs:t.inputs (Passes.Uniquify.uniquify p.e))
    ~msg:("uniquify: " ^ t.message) ~printer:string_of_int;
  assert_equal true
    (p.e |> Passes.Uniquify.uniquify |> Passes.Uniquify.is_uniquify)
    ~msg:("is_uniquify: " ^ t.message) ~printer:string_of_bool;
  assert_equal t.value
    (interp ~inputs:t.inputs (p.e |> Passes.Uniquify.uniquify |> Passes.Resolve_complex.resolve_complex))
    ~msg:("uniquify |> resolve_complex: " ^ t.message)
    ~printer:string_of_int;
  assert_equal true
    (p.e |> Passes.Uniquify.uniquify |> Passes.Resolve_complex.resolve_complex
   |> Passes.Resolve_complex.is_resolve_complex)
    ~msg:("is_resolve_complex: " ^ t.message) ~printer:string_of_bool;
  assert_equal t.value
    (C0.Interp.interp ~inputs:t.inputs
       (p.e |> Passes.Uniquify.uniquify |> Passes.Resolve_complex.resolve_complex
      |> Passes.Explicate_control.explicate_control))
    ~msg:("uniquify |> resolve_complex |> explicate_control: " ^ t.message)
    ~printer:string_of_int

let test_optimize (t : test) =
  (* non-optimized expr string *)
  let lexbuf_expr = Lexing.from_string t.expr in
  (* program with non-optimized expr *)
  let p_expr = make_prog lexbuf_expr in
  (* optimized expr string *)
  let lexbuf_opt = Lexing.from_string t.optimized in
  (* program with optimized expr string *)
  let p_opt = make_prog lexbuf_opt in
  (* optimized expr *)
  let p_expr_ast : R1.Ast.expr = optimize p_expr.e in
  let p_opt_ast : R1.Ast.expr = optimize p_opt.e in
  assert_equal (interp ~inputs:t.inputs p_opt.e) (interp ~inputs:t.inputs p_expr.e)
    ~msg:("optimize vs. non-optimize: " ^ t.message)
    ~printer:string_of_int;
  assert_equal p_opt_ast p_expr_ast ~msg:("optimize AST vs. expr optimize AST: " ^ t.message) ~printer:R1.Pp.pp;
  assert_equal (interp ~inputs:t.inputs p_opt.e)
    (interp ~inputs:t.inputs (Passes.Uniquify.uniquify p_expr.e))
    ~msg:("uniquify: optimize vs. non-optimize: " ^ t.message)
    ~printer:string_of_int;
  assert_equal (interp ~inputs:t.inputs p_opt.e)
    (interp ~inputs:t.inputs (p_expr.e |> Passes.Uniquify.uniquify |> Passes.Resolve_complex.resolve_complex))
    ~msg:("uniquify |> resolve_complex: optimize vs. non-optimize: " ^ t.message)
    ~printer:string_of_int;
  assert_equal (interp ~inputs:t.inputs p_opt.e)
    (C0.Interp.interp ~inputs:t.inputs
       (p_expr.e |> Passes.Uniquify.uniquify |> Passes.Resolve_complex.resolve_complex
      |> Passes.Explicate_control.explicate_control))
    ~msg:("uniquify |> resolve_complex |> explicate_control: optimize vs. non-optimize: " ^ t.message)
    ~printer:string_of_int
