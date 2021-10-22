let fresh_var =
  let counter = ref 0 in
  fun () ->
    let x = "x" ^ string_of_int !counter in
    counter := !counter + 1;
    x
