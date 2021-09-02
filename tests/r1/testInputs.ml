open TestUtils
open R1.Generator
open R1.Interp
open Repl

let var_example = {
  expr = "(let ([x 1]) x)";
  optimized = "1";
  interp = 1;
  input = [];
  message = "var (1)"
}

let var_list = [var_example]

(* random *)
let randp n =
  let ast = randp n in
  let expr = pp ast in
  let optimized = pp (optimize ast) in
  let input = generate_input_for_randp ast in
  let interp = interp ast (make_read input) in
  let message = "randp" ^ string_of_int n in
  {
    expr = expr;
    optimized = optimized;
    interp = interp;
    input = input;
    message = message
  }

