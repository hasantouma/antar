open Ast
open Parser.Main

let lex_parse (lexbuf : Lexing.lexbuf) : program option =
  try Some (make_prog lexbuf) with
  | Exceptions.BadInput s ->
    print_endline ("Exception! Bad Input: " ^ s);
    None

let make_read (readings : int list) : unit -> int =
  match readings with
  | [] ->
    fun () ->
      output_string stdout "In *> ";
      flush stdout;
      read_int ()
  | _ ->
    let box = ref readings in
    let rec f () =
      match !box with
      | [] ->
        box := readings;
        f ()
      | head :: tail ->
        box := tail;
        head
    in
    f

let parse_file (name : string) : program option =
  let chan = open_in name in
  let lexbuf = Lexing.from_channel chan in
  let p : program option = lex_parse lexbuf in
  close_in chan;
  p

let handle_exit repl_in = if repl_in = "#quit" then exit 0

let handle_display (p : program option) : unit =
  match p with
  | None -> print_endline "Invalid program"
  | Some p -> (
    try
      let n = R1.Interp.interp p.e in
      let answer = string_of_int n in
      print_endline (R1.Pp.pp p.e ^ " -> " ^ answer)
    with
    | Not_found -> print_endline "Can't find var in env.")

let rec repl () : unit =
  output_string stdout "> ";
  flush stdout;
  let repl_in = input_line stdin in
  handle_exit repl_in;
  let lexbuf = Lexing.from_string repl_in in
  let p : program option = lex_parse lexbuf in
  handle_display p;
  repl ()

let interp_file (file_name : string) : unit =
  if Sys.file_exists file_name then
    let p : program option = parse_file file_name in
    handle_display p
  else (
    print_endline "File does not exist!";
    exit 1
  )

(* TODO: This feature is not connected to the repl right now *)
let interp_stdin (s : string) : unit =
  let lexbuf = Lexing.from_string s in
  let p : program option = lex_parse lexbuf in
  handle_display p

let visualize (file_name : string) : unit =
  if Sys.file_exists file_name then (
    let p : program option = parse_file file_name in
    handle_display p;
    Viz.write_expr_to_graphviz p
  ) else
    interp_stdin file_name

let randp (viz : bool) (n : int) : unit =
  let r : R1.Ast.expr = R1.Generator.randp n in
  let p = { info = false; e = r } in
  handle_display (Some p);
  if viz then
    Viz.write_expr_to_graphviz (Some p)
  else
    ()
