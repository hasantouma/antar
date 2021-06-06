open Repl

let () =
  let args_len = Array.length Sys.argv in
  if args_len > 1 then
    interp_file Sys.argv.(1)
  else
    begin
      print_endline "Welcome to the 'Antar' REPL";
      repl ()
    end

