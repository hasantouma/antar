open OUnit2
open Parser.Main
open R1.Interp
open Repl

type test =
  { expr : string
  ; optimized : string
  ; interp : int
  ; input : int list
  ; message : string
  }

let test_interp (t : test) =
  let lexbuf = Lexing.from_string t.expr in
  let p = make_prog lexbuf in
  let input = make_read t.input in
  assert_equal t.interp (interp p.e ~input) ~msg:("interp: " ^ t.message) ~printer:string_of_int

let test_optimize (t : test) =
  let lexbuf_expr = Lexing.from_string t.expr in
  let p_expr = make_prog lexbuf_expr in
  let lexbuf_opt = Lexing.from_string t.optimized in
  let p_opt = make_prog lexbuf_opt in
  let input = make_read t.input in
  assert_equal (interp p_opt.e ~input) (interp p_expr.e ~input) ~msg:("optimize: " ^ t.message) ~printer:string_of_int

let pp_list (lst : int list) : string =
  let s = List.fold_left (fun acc h -> acc ^ string_of_int h ^ "; ") "" lst in
  let len = String.length s in
  if len > 1 then
    let s' = String.sub s 0 (len - 2) in
    "[" ^ s' ^ "]"
  else
    "[]"
