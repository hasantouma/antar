open TestUtils
open Repl

let var_1 = {
  expr = "(let ([x 1]) x)";
  optimized = "1";
  interp = 1;
  input = [];
  message = "var (1)"
}

let var_shadow = {
  expr = "(let ([x (let ([x 2]) x)]) x)";
  optimized = "2";
  interp = 2;
  input = [];
  message = "var (2)"
}

let var_name = {
  expr = "(let ([x-1+program (let ([x 2]) x)]) x-1+program)";
  optimized = "2";
  interp = 2;
  input = [];
  message = "var (3)"
}

let var_list = [var_1; var_shadow; var_name]

let let_read_order = {
  expr = "(let ([x (read)]) (+ x (- (read))))";
  optimized = "(let ([x (read)]) (+ x (- (read))))";
  interp = 40;
  input = [42; 2];
  message = "let (1)"
}

let let_number_read = {
  expr = "(let ([x (read)]) (+ (+ x x) (- (read))))";
  optimized = "(let ([x (read)]) (+ (+ x x) (- (read))))";
  interp = 0;
  input = [5; 10];
  message = "let (2)"
}

let let_list = [let_read_order; let_number_read]

(* random *)
let randp n =
  let ast = R1.Generator.randp n in
  let expr = R1.Pp.pp ast 0 in
  let optimized = R1.Pp.pp (R1.Interp.optimize ast) 0 in
  let input = R1.Generator.generate_input_for_randp ast in
  let interp = R1.Interp.interp ast [] (make_read input) in
  let message = "randp" ^ string_of_int n in
  {
    expr = expr;
    optimized = optimized;
    interp = interp;
    input = input;
    message = message
  }

