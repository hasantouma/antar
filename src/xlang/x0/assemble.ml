let handle_status (stat : Unix.process_status) : unit =
  match stat with
  | Unix.WEXITED n when n <> 0 -> raise (Failure (Printf.sprintf "exec_command WEXITED Error: %d" n))
  | Unix.WSIGNALED n -> raise (Failure (Printf.sprintf "exec_command WSIGNALED Error: %d" n))
  | Unix.WSTOPPED n -> raise (Failure (Printf.sprintf "exec_command WSTOPPED Error: %d" n))
  | _ -> ()

let chomp s =
  let n = String.length s in
  if n > 0 && s.[n - 1] = '\n' then
    String.sub s 0 (n - 1)
  else
    s

let unwind (protect : 'a -> unit) f x =
  try
    let y = f x in
    protect x;
    y
  with
  | e ->
    protect x;
    raise e

let input_to_out_channel (out_chan : out_channel) (input : string list) : unit =
  List.iter
    (fun i ->
      output_string out_chan (i ^ "\n");
      flush out_chan)
    input

let exec_command ?(input = []) (command : string) : string =
  let (in_chan, out_chan) : in_channel * out_channel = Unix.open_process command in
  input_to_out_channel out_chan input;
  let res = Core.In_channel.input_all in_chan in
  handle_status (Unix.close_process (in_chan, out_chan));
  res

let assemble ?(input = []) (p : Ast.p) : string =
  let str : string = Emit.emitp true p in
  let file, out = Filename.open_temp_file "" ".s" in
  output_string out str;
  close_out out;
  unwind Sys.remove
    (fun name ->
      let _ = exec_command (Printf.sprintf "as -o %s.o %s" name name) in
      let _ = exec_command (Printf.sprintf "cc -o runtime_exec ../../../../../runtime/runtime.o %s.o" name) in
      let res = exec_command ~input "./runtime_exec" in
      chomp res)
    file
