open OUnit2
open Parser.Main
open R1.Interp

type test =
  { expr : string
  ; optimized : string
  ; interp : int
  ; inputs : int list
  ; message : string
  }

let test_interp (t : test) =
  let lexbuf = Lexing.from_string t.expr in
  let p = make_prog lexbuf in
  assert_equal t.interp (interp p.e ~inputs:t.inputs) ~msg:("interp: " ^ t.message) ~printer:string_of_int

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
  let p_expr_opt : R1.Ast.expr = optimize p_expr.e in
  let p_opt_opt : R1.Ast.expr = optimize p_opt.e in
  assert_equal (interp p_opt.e ~inputs:t.inputs) (interp p_expr.e ~inputs:t.inputs)
    ~msg:("optimize vs. non-optimize: " ^ t.message)
    ~printer:string_of_int;
  assert_equal p_opt_opt p_expr_opt ~msg:("opt-optimize vs. optimize: " ^ t.message) ~printer:R1.Pp.pp
