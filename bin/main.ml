open Pretty_print
open Lex_parse.Main
open Repl
open Interp
open Generator
open Utils

let () =
  let e5 = exp2 5 in
  print_endline ("exp5: " ^ (pp e5));
  print_endline ("interp exp5: " ^ (string_of_int (interp e5 (make_read []))))


let () =
  let r3 = randp 3 in
  print_endline ("randp3: " ^ (pp r3));
  print_endline ("interp randp3: " ^ (string_of_int (interp r3 (make_read []))))

let () =
  let p = parse_file "src/ex1.ht" in
  print_endline (pp p.e)

let () = repl ()

