open TestUtils
open Repl

let var_1 = { expr = "(let ([x 1]) x)"; optimized = "1"; interp = 1; input = []; message = "var (1)" }

let var_shadow =
  { expr = "(let ([x (let ([x 2]) x)]) x)"; optimized = "2"; interp = 2; input = []; message = "var (2)" }

let body_shadow =
  { expr = "(let ([x 1]) (let ([x 2]) x))"; optimized = "2"; interp = 2; input = []; message = "var (3)" }

let var_name =
  { expr = "(let ([x-1+program (let ([x 2]) x)]) x-1+program)"
  ; optimized = "2"
  ; interp = 2
  ; input = []
  ; message = "var (4)"
  }

let var_list = [ var_1; var_shadow; body_shadow; var_name ]

let let_read_order =
  { expr = "(let ([x (read)]) (+ x (- (read))))"
  ; optimized = "(let ([x (read)]) (+ x (- (read))))"
  ; interp = 40
  ; input = [ 42; 2 ]
  ; message = "let (1)"
  }

let let_number_read =
  { expr = "(let ([x (read)]) (+ (+ x x) (- (read))))"
  ; optimized = "(let ([x (read)]) (+ (+ x x) (- (read))))"
  ; interp = 0
  ; input = [ 5; 10 ]
  ; message = "let (2)"
  }

let let_no_opt =
  { expr = "(let ([x0 (let ([x1 (+ (read) 504)]) (- x1))]) (let ([x2 (- x0)]) (+ x0 x2)))"
  ; optimized = "(let ([x0 (let ([x1 (+ (read) 504)]) (- x1))]) (let ([x2 (- x0)]) (+ x0 x2)))"
  ; interp = 0
  ; input = [ 1 ]
  ; message = "let (3)"
  }

let let_simple_opt =
  { expr = "(let ([a (read)]) (+ a (- a)))"
  ; optimized = "(let ([a (read)]) (+ a (- a)))"
  ; interp = 0
  ; input = [ 1 ]
  ; message = "let (4)"
  }

let let_nested_let =
  { expr = "(let ([v0 (let ([v1 2]) (+ v1 3))]) (- (+ v0 (read))))"
  ; optimized = "(+ -5 (- (read)))"
  ; interp = -7
  ; input = [ 2 ]
  ; message = "let (5)"
  }

let let_list = [ let_read_order; let_number_read; let_no_opt; let_simple_opt; let_nested_let ]

(* random *)
let randp n =
  let ast = R1.Generator.randp n in
  let expr = R1.Pp.pp ast in
  let optimized = R1.Pp.pp (R1.Interp.optimize ast) in
  let input = R1.Generator.generate_input_for_randp ast in
  let interp = R1.Interp.interp ast ~input:(make_read input) in
  let message = "randp" ^ string_of_int n ^ ":\nInput: " ^ pp_list input ^ "\nExpr:" ^ expr in
  { expr; optimized; interp; input; message }
