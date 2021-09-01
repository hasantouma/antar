open TestUtils

(* int *)
let one = {
  ast = `EInt 1;
  optimized = `EInt 1;
  interp = 1;
  input = [];
  message = "int (1)"
}

let forty_two = {
  ast = `EInt 42;
  optimized = `EInt 42;
  interp = 42;
  input = [];
  message = "int (2)"
}

let int_list = [one; forty_two]

(* read *)
let read = {
  ast = `ERead;
  optimized = `ERead;
  interp = 23;
  input = [23];
  message = "read (1)"
}

let read_list = [read]

(* add *)
let add_1_2 = {
  ast = `EAdd (`EInt 1, `EInt 2);
  optimized = `EInt 3;
  interp = 3;
  input = [];
  message = "add (1)"
}

let add_33_read = {
  ast = `EAdd (`EInt 33, `ERead);
  optimized = `EAdd (`EInt 33, `ERead);
  interp = 36;
  input = [3];
  message = "add (2)"
}

let add_read_72 = {
  ast = `EAdd (`ERead, `EInt 72);
  optimized = `EAdd (`ERead, `EInt 72);
  interp = 80;
  input = [8];
  message = "add (3)"
}

let add_read_read = {
  ast = `EAdd (`ERead, `ERead);
  optimized = `EAdd (`ERead, `ERead);
  interp = 5;
  input = [2; 3];
  message = "add (4)"
}

let add_list = [add_1_2; add_33_read; add_read_72; add_read_read]

(* negate *)
let negate_1 = {
  ast = `ENegate (`EInt 1);
  optimized = `EInt (-1);
  interp = -1;
  input = [];
  message = "negate (1)"
}

let negate_read = {
  ast = `ENegate (`ERead);
  optimized = `ENegate (`ERead);
  interp = -12;
  input = [12];
  message = "negate (2)"
}

let add_read_and_negate_72 = {
  ast = `EAdd (`ERead, `ENegate (`EInt 72));
  optimized = `EAdd (`ERead, `EInt (-72));
  interp = -72;
  input = [0];
  message = "negate (3)"
}

let negate_add_99_50 = {
  ast = `ENegate (`EAdd (`EInt 99, `EInt 50));
  optimized = `EInt (-149);
  interp = -149;
  input = [];
  message = "negate (4)"
}

let negate_negate_5 = {
  ast = `ENegate (`ENegate (`EInt 5));
  optimized = `EInt 5;
  interp = 5;
  input = [];
  message = "negate (5)"
}

let negate_list = [negate_1; negate_read; add_read_and_negate_72; negate_add_99_50; negate_negate_5]

(* random *)
let randp3 () =
  let ast = R0.Generator.randp 3 in
  let optimized = R0.Interp.optimize ast in
  let input = R0.Generator.generate_input_for_randp ast in
  let interp = R0.Interp.interp ast (Repl.make_read input) in
  let message = "randp3" in
  {
    ast = ast;
    optimized = optimized;
    interp = interp;
    input = input;
    message = message
  }

