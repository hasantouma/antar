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

let exec_command (command : string) : string * Unix.process_status =
  let chan = Unix.open_process_in command in
  let res = Core.In_channel.input_all chan in
  let stat = Unix.close_process_in chan in
  match stat with
  | Unix.WEXITED n when n <> 0 -> raise (Failure (Printf.sprintf "exec_command WEXITED Error: %d" n))
  | Unix.WSIGNALED n -> raise (Failure (Printf.sprintf "exec_command WSIGNALED Error: %d" n))
  | Unix.WSTOPPED n -> raise (Failure (Printf.sprintf "exec_command WSTOPPED Error: %d" n))
  | _ -> (res, stat)

(* Get value from RAX *)
let assemble ?(input = []) (p : Ast.p) : string =
  let _read_int : unit -> int = Utils.Repl.make_read input in
  let str : string = Emit.emitp true p in
  let file, out = Filename.open_temp_file "" ".s" in
  output_string out str;
  close_out out;
  unwind Sys.remove
    (fun name ->
      let _ = exec_command (Printf.sprintf "as -o %s.o %s" name name) in
      let _ = exec_command (Printf.sprintf "cc -o runtime_exec ../../../../../runtime/runtime.o %s.o" name) in
      let res, _ = exec_command "echo 52 | ./runtime_exec" in
      chomp res)
    file
