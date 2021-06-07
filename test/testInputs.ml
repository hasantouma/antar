open TestUtils
open Generator
open Pretty_print
open Interp

(* int *)
let one = {
  ast = `EInt 1;
  pp = "1";
  interp = 1;
  input = [];
  message = "int (1)"
}

let fourty_two = {
  ast = `EInt 42;
  pp = "42";
  interp = 42;
  input = [];
  message = "int (2)"
}

(* read *)
let read = {
  ast = `ERead;
  pp = "(read)";
  interp = 23;
  input = [23];
  message = "read (1)"
}


(* add *)
let add_1_2 = {
  ast = `EAdd (`EInt 1, `EInt 2);
  pp = "(+ 1 2)";
  interp = 3;
  input = [];
  message = "add (1)"
}

let add_33_read = {
  ast = `EAdd (`EInt 33, `ERead);
  pp = "(+ 33 (read))";
  interp = 36;
  input = [3];
  message = "add (2)"
}

let add_read_72 = {
  ast = `EAdd (`ERead, `EInt 72);
  pp = "(+ (read) 72)";
  interp = 80;
  input = [8];
  message = "add (3)"
}

let add_read_read = {
  ast = `EAdd (`ERead, `ERead);
  pp = "(+ (read) (read))";
  interp = 5;
  input = [2; 3];
  message = "add (4)"
}


(* negate *)
let negate_1 = {
  ast = `ENegate (`EInt 1);
  pp = "(- 1)";
  interp = -1;
  input = [];
  message = "negate (1)"
}

let negate_read = {
  ast = `ENegate (`ERead);
  pp = "(- (read))";
  interp = -12;
  input = [12];
  message = "negate (2)"
}

let add_read_and_negate_72 = {
  ast = `EAdd (`ERead, `ENegate (`EInt 72));
  pp = "(+ (read) (- 72))";
  interp = -72;
  input = [0];
  message = "negate (3)"
}

let negate_add_99_50 = {
  ast = `ENegate (`EAdd (`EInt 99, `EInt 50));
  pp = "(- (+ 99 50))";
  interp = -149;
  input = [];
  message = "negate (4)"
}

let negate_negate_5 = {
  ast = `ENegate (`ENegate (`EInt 5));
  pp = "(- (- 5))";
  interp = 5;
  input = [];
  message = "negate (5)"
}

let randp3 () =
  let ast = randp 3 in
  let pp = pp ast in
  let input = generate_input_for_randp ast in
  let interp = interp ast input in
  let message = "randp3" in
  {
    ast = ast;
    pp = pp;
    interp = interp;
    input = input;
    message = message
  }

