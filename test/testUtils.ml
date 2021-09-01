open OUnit2

type test = {
  ast       : R0.Ast.expr;
  optimized : R0.Ast.expr;
  interp    : int;
  input     : int list;
  message   : string
}

let test_interp (t : test) =
  assert_equal
    t.interp (R0.Interp.interp t.ast (Repl.make_read t.input))
    ~msg:("interp: " ^ t.message) ~printer:string_of_int

let test_optimize (t : test) =
  assert_equal
    t.optimized (R0.Interp.optimize t.ast)
    ~msg:("optimize: " ^ t.message) ~printer:(fun e -> R0.Pp.pp e 0)

