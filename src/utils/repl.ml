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
