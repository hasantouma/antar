open Rlang

let lex_parse (lexbuf : Lexing.lexbuf) : rprogram option =
  try Some (parse lexbuf) with
  | SyntaxError s ->
    print_endline ("Syntax Error: " ^ s);
    None
  | Parsing.Parse_error ->
    print_endline "Parsing Error";
    None

let parse_file (name : string) : rprogram option =
  let chan = open_in name in
  let lexbuf = Lexing.from_channel chan in
  let rprog : rprogram option = lex_parse lexbuf in
  close_in chan;
  rprog

let handle_exit repl_in = if repl_in = "#quit" then exit 0

let handle_display (rprog : rprogram option) : unit =
  match rprog with
  | None -> print_endline "Invalid program"
  | Some rprog -> (
    try
      let n = interp rprog in
      let answer = string_of_int n in
      print_endline (pp rprog ^ " -> " ^ answer)
    with Not_found -> print_endline "Can't find var in env.")

let rec repl () : unit =
  output_string stdout "> ";
  flush stdout;
  let repl_in = input_line stdin in
  handle_exit repl_in;
  let lexbuf = Lexing.from_string repl_in in
  let rprog : rprogram option = lex_parse lexbuf in
  handle_display rprog;
  repl ()

let interp_file (file_name : string) : unit =
  if Sys.file_exists file_name
  then
    let rprog : rprogram option = parse_file file_name in
    handle_display rprog
  else (
    print_endline "File does not exist!";
    exit 1)

let extract_input_file (input_file : string) : string =
  match Filename.chop_suffix_opt input_file ~suffix:".ht" with
  | Some x -> x
  | None -> input_file

let compile ~(input_file : string) ~(output_file : string) ~(output_assembly : bool) ~(passes : Passes.pass list) : unit
    =
  if Sys.file_exists input_file
  then
    let rprog_opt : rprogram option = parse_file input_file in
    let input_file = extract_input_file input_file in
    match rprog_opt with
    | None -> print_endline "No file to compile!"
    | Some rprog ->
      let _ = Passes.process_passes ~input_file ~rprog ~passes in
      let xprog : Xlang.xprogram = Passes.passes rprog in
      let _ = Xlang.assemble ~input_file ~output_file ~output_assembly xprog in
      ()
  else (
    print_endline "File does not exist!";
    exit 1)

(* TODO: This feature is not connected to the repl right now *)
let interp_stdin (s : string) : unit =
  let lexbuf = Lexing.from_string s in
  let rprog : rprogram option = lex_parse lexbuf in
  handle_display rprog

let visualize (file_name : string) : unit =
  if Sys.file_exists file_name
  then (
    let rprog : rprogram option = parse_file file_name in
    handle_display rprog;
    let rprog = Option.get rprog in
    Viz.write_expr_to_graphviz rprog.e)
  else interp_stdin file_name

let randp (viz : bool) (n : int) : unit =
  let rprog : rprogram = randp n in
  handle_display (Some rprog);
  if viz then Viz.write_expr_to_graphviz rprog.e else ()
