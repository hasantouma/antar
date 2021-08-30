open TestUtils

(* int *)
let one = {
  ast = `EInt 1;
  interp = 1;
  input = [];
  message = "int (1)"
}

let fourty_two = {
  ast = `EInt 42;
  interp = 42;
  input = [];
  message = "int (2)"
}

(* read *)
let read = {
  ast = `ERead;
  interp = 23;
  input = [23];
  message = "read (1)"
}


(* add *)
let add_1_2 = {
  ast = `EAdd (`EInt 1, `EInt 2);
  interp = 3;
  input = [];
  message = "add (1)"
}

let add_33_read = {
  ast = `EAdd (`EInt 33, `ERead);
  interp = 36;
  input = [3];
  message = "add (2)"
}

let add_read_72 = {
  ast = `EAdd (`ERead, `EInt 72);
  interp = 80;
  input = [8];
  message = "add (3)"
}

let add_read_read = {
  ast = `EAdd (`ERead, `ERead);
  interp = 5;
  input = [2; 3];
  message = "add (4)"
}


(* negate *)
let negate_1 = {
  ast = `ENegate (`EInt 1);
  interp = -1;
  input = [];
  message = "negate (1)"
}

let negate_read = {
  ast = `ENegate (`ERead);
  interp = -12;
  input = [12];
  message = "negate (2)"
}

let add_read_and_negate_72 = {
  ast = `EAdd (`ERead, `ENegate (`EInt 72));
  interp = -72;
  input = [0];
  message = "negate (3)"
}

let negate_add_99_50 = {
  ast = `ENegate (`EAdd (`EInt 99, `EInt 50));
  interp = -149;
  input = [];
  message = "negate (4)"
}

let negate_negate_5 = {
  ast = `ENegate (`ENegate (`EInt 5));
  interp = 5;
  input = [];
  message = "negate (5)"
}

let randp3 () =
  let ast = R0_generator.randp 3 in
  let input = R0_generator.generate_input_for_randp ast in
  let interp = R0_interp.interp ast (Repl.make_read input) in
  let message = "randp3" in
  {
    ast = ast;
    interp = interp;
    input = input;
    message = message
  }

