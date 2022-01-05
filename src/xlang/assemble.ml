let runtime_obj_string = [%blob "runtime.o"]

let handle_status (stat : Unix.process_status) : unit =
  match stat with
  | Unix.WEXITED n when n <> 0 -> raise (Failure (Printf.sprintf "exec_command WEXITED Error: %d" n))
  | Unix.WSIGNALED n -> raise (Failure (Printf.sprintf "exec_command WSIGNALED Error: %d" n))
  | Unix.WSTOPPED n -> raise (Failure (Printf.sprintf "exec_command WSTOPPED Error: %d" n))
  | _ -> ()

let chomp s =
  let n = String.length s in
  if n > 0 && s.[n - 1] = '\n' then String.sub s 0 (n - 1) else s

let unwind protect f x =
  try
    let y = f x in
    let _ = protect x in
    y
  with e ->
    let _ = protect x in
    raise e

let inputs_to_out_channel (out_chan : out_channel) (inputs : string list) : unit =
  List.iter
    (fun i ->
      output_string out_chan (i ^ "\n");
      flush out_chan)
    inputs

let exec_command ?(inputs = []) (command : string) : string =
  let (in_chan, out_chan) : in_channel * out_channel = Unix.open_process command in
  inputs_to_out_channel out_chan inputs;
  let result = Core.In_channel.input_all in_chan in
  handle_status (Unix.close_process (in_chan, out_chan));
  result

let assemble ?(inputs = []) ?(run = false) ?(output_file = "a.out") (xprog : Ast.xprogram) : string =
  let str : string = Emit.emitp false xprog in
  let assembly_file, assembly_out = Filename.open_temp_file "" ".s" in
  let runtime_file, runtime_out = Filename.open_temp_file "" ".o" in
  let bin = if run then Filename.temp_file "" ".out" else output_file in
  output_string assembly_out str;
  output_string runtime_out runtime_obj_string;
  close_out assembly_out;
  close_out runtime_out;
  unwind (List.map Sys.remove)
    (fun _ ->
      let _ = exec_command (Printf.sprintf "as -o %s.o %s" assembly_file assembly_file) in
      let _ = exec_command (Printf.sprintf "cc -o %s -O0 %s %s.o" bin runtime_file assembly_file) in
      let result : string = if run then exec_command ~inputs bin else "" in
      chomp result)
    [ assembly_file; runtime_file ]
