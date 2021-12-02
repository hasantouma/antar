open OUnit2

type rtest =
  { expr : string
  ; optimized : string
  ; value : int
  ; inputs : int list
  ; message : string
  }

let make_rprog (e : string) : R1.Lang.rprogram = e |> Lexing.from_string |> R1.Lang.parse

let passes_pipe (expr : R1.Ast.expr) : C0.Ast.cprogram =
  expr |> R1.Interp.optimize |> Passes.Uniquify.uniquify |> Passes.Resolve_complex.resolve_complex
  |> Passes.Explicate_control.explicate_control

(* *** Testing without optimized pass *** *)

let test_interp (t : rtest) =
  let p = make_rprog t.expr in
  assert_equal t.value (R1.Interp.interp ~inputs:t.inputs p.e) ~msg:("interp: " ^ t.message) ~printer:string_of_int;
  assert_equal t.value
    (C0.Interp.interp ~inputs:t.inputs (passes_pipe p.e))
    ~msg:("passes_pipe: " ^ t.message) ~printer:string_of_int

(* *** Testing with optimized pass *** *)

let test_optimize (t : rtest) =
  let p_expr = make_rprog t.expr in
  let p_optimized = make_rprog t.optimized in
  assert_equal
    (R1.Interp.interp ~inputs:t.inputs p_optimized.e)
    (R1.Interp.interp ~inputs:t.inputs p_expr.e)
    ~msg:("optimize vs. non-optimize: " ^ t.message)
    ~printer:string_of_int;
  assert_equal p_optimized.e (R1.Interp.optimize p_expr.e)
    ~msg:("optimize AST vs. expr optimize AST: " ^ t.message)
    ~printer:R1.Pp.pp
