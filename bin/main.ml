open Pretty_print
open Lex_parse.Main
open Repl
open Interp
open Generator
open Utils

let () =
  let e3 = exp2 3 in
  print_endline ("exp of 3: " ^ (pp e3));
  print_endline ("exp of 3 with indent: " ^ (pp_with_indent e3));
  print_endline ("interp exp of 3: " ^ (string_of_int (interp e3 (make_read []))))


let () =
  let r3 = randp 3 in
  let p3 = pp r3 in
  let indent_p3 = pp_with_indent r3 in
  print_endline ("randp3: " ^ p3);
  print_endline ("indent_p3" ^ indent_p3);
  print_endline ("interp randp3: " ^ (string_of_int (interp r3 (make_read []))))

let () =
  let p = parse_file "src/ex1.ht" in
  print_endline (pp p.e);
  print_endline (pp_with_indent p.e)

let () = repl ()

