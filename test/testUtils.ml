open OUnit2

type test = {
  ast     : R0.Ast.expr;
  interp  : int;
  input   : int list;
  message : string
}

let test_interp (t : test) = assert_equal t.interp (R1.Interp.interp t.ast (Repl.make_read t.input)) ~msg:("interp: " ^ t.message) ~printer:string_of_int

