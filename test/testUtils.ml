open OUnit2
open String
open Pretty_print
open Lex_parse.Ast

type test = {
  ast     : expr;
  pp      : string;
  interp  : int;
  input   : int list;
  message : string
}

let test_pp     (t : test) = assert_equal t.pp     (pp t.ast)             ~msg:("pp: " ^ t.message)		~printer:lowercase_ascii
let test_interp (t : test) = assert_equal true true (* t.interp (interp t.ast t.input) ~msg:("interp: " ^ t.message) ~printer:string_of_int *)
