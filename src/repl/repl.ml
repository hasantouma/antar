open Ast
open Parser.Main

let make_read (readings : int list) : (unit -> int) =
  match readings with
  | [] ->
      (fun () ->
        output_string stdout "In *> ";
        flush stdout;
        read_int ())
  | _ ->
      let box = ref readings in
      let rec f () =
        match !box with
        | [] -> box := readings; f ()
        | head :: tail -> box := tail; head
      in f

let parse_file (name : string) : program =
  let chan = open_in name in
  let lexbuf = Lexing.from_channel chan in
  let p : program = make_prog lexbuf in
  close_in chan;
  p

let handle_exit repl_in =
  if repl_in = "#quit" then
    exit 0

let rec repl () : unit =
  output_string stdout "> ";
  flush stdout;
  let repl_in = (input_line stdin) in
  handle_exit repl_in;
  let lexbuf = Lexing.from_string repl_in in
  let p : program = make_prog lexbuf in
  print_endline (string_of_int (R1.Interp.interp p.e (Utils.make_read [])));
  repl ()

let interp_file (file_name : string) : unit =
  let p : program = parse_file file_name in
  print_endline (R1.Pp.pp p.e);
  let i = R1.Interp.interp p.e (Utils.make_read []) in
  print_endline (string_of_int i)

