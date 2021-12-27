open Rlang

let lex_parse (lexbuf : Lexing.lexbuf) : rprogram option =
  try Some (parse lexbuf)
  with Exceptions.BadInput s ->
    print_endline ("Exception! Bad Input: " ^ s);
    None

let parse_file (name : string) : rprogram option =
  let chan = open_in name in
  let lexbuf = Lexing.from_channel chan in
  let p : rprogram option = lex_parse lexbuf in
  close_in chan;
  p

let handle_exit repl_in = if repl_in = "#quit" then exit 0

let handle_display (p : rprogram option) : unit =
  match p with
  | None -> print_endline "Invalid rprogram"
  | Some p -> (
    try
      let n = interp p.e in
      let answer = string_of_int n in
      print_endline (pp p.e ^ " -> " ^ answer)
    with Not_found -> print_endline "Can't find var in env.")

let rec repl () : unit =
  output_string stdout "> ";
  flush stdout;
  let repl_in = input_line stdin in
  handle_exit repl_in;
  let lexbuf = Lexing.from_string repl_in in
  let p : rprogram option = lex_parse lexbuf in
  handle_display p;
  repl ()

let interp_file (file_name : string) : unit =
  if Sys.file_exists file_name
  then
    let p : rprogram option = parse_file file_name in
    handle_display p
  else (
    print_endline "File does not exist!";
    exit 1)

(* TODO: This feature is not connected to the repl right now *)
let interp_stdin (s : string) : unit =
  let lexbuf = Lexing.from_string s in
  let p : rprogram option = lex_parse lexbuf in
  handle_display p

let visualize (file_name : string) : unit =
  if Sys.file_exists file_name
  then (
    let p : rprogram option = parse_file file_name in
    handle_display p;
    let p = Option.get p in
    Viz.write_expr_to_graphviz p.e)
  else interp_stdin file_name

let randp (viz : bool) (n : int) : unit =
  let r : expr = randp n in
  let p = make_prog r in
  handle_display (Some p);
  if viz then Viz.write_expr_to_graphviz p.e else ()
