let make_read (readings : int list) : unit -> int =
  match readings with
  | [] ->
    fun () ->
      output_string stdout "In *> ";
      flush stdout;
      read_int ()
  | _ -> (
    let box = ref readings in
    fun () ->
      match !box with
      | [] -> raise Exceptions.TooManyReads
      | head :: tail ->
        box := tail;
        head)
