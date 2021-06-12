open OUnit2
open String

type test = {
  ast     : R0.Ast.expr;
  pp      : string;
  interp  : int;
  input   : int list;
  message : string
}

let test_pp     (t : test) = assert_equal t.pp     (R1.Pp.pp t.ast)             ~msg:("pp: " ^ t.message)		~printer:lowercase_ascii
let test_interp (t : test) = assert_equal t.interp (R1.Interp.interp t.ast (Repl.make_read t.input)) ~msg:("interp: " ^ t.message) ~printer:string_of_int

