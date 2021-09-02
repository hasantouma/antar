open OUnit2
open Parser.Main
open R1.Ast
open R1.Interp
open Repl

type test = {
  expr       : string;
  optimized  : string;
  interp     : int;
  read_input : int list;
  message    : string
}

let test_interp (t : test) =
  let lexbuf = Lexing.from_string t.expr in
  let p : program = make_prog lexbuf in
  assert_equal
    t.interp (interp p.e (make_read t.input))
    ~msg:("interp: " ^ t.message) ~printer:string_of_int

let test_optimize (t : test) =
  let lexbuf_expr = Lexing.from_string t.expr in
  let p_expr : program = make_prog lexbuf_expr in
  let lexbuf_opt = Lexing.from_string t.optimized in
  let p_opt : program = make_prog lexbuf_opt in
  let input = make_read t.input in
  assert_equal
    (interp p_opt.e input) (interp p_expr.e input)
    ~msg:("optimize: " ^ t.message) ~printer:string_of_int

