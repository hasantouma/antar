open TestUtils

(* int *)
let one = { expr = "1"; optimized = "1"; value = 1; inputs = []; message = "int (1)" }
let forty_two = { expr = "42"; optimized = "42"; value = 42; inputs = []; message = "int (2)" }
let int_list = [ one; forty_two ]

(* read *)
let read = { expr = "(read)"; optimized = "(read)"; value = 23; inputs = [ 23 ]; message = "read (1)" }
let read_list = [ read ]

(* add *)
let add_1_2 = { expr = "(+ 1 2)"; optimized = "3"; value = 3; inputs = []; message = "add (1)" }

let add_33_read =
  { expr = "(+ 33 (read))"; optimized = "(+ 33 (read))"; value = 36; inputs = [ 3 ]; message = "add (2)" }

let add_read_72 =
  { expr = "(+ (read) 72)"; optimized = "(+ 72 (read))"; value = 80; inputs = [ 8 ]; message = "add (3)" }

let add_read_read =
  { expr = "(+ (read) (read))"; optimized = "(+ (read) (read))"; value = 5; inputs = [ 2; 3 ]; message = "add (4)" }

let add_list = [ add_1_2; add_33_read; add_read_72; add_read_read ]

(* negate *)
let negate_1 = { expr = "(- 1)"; optimized = "-1"; value = -1; inputs = []; message = "negate (1)" }

let negate_read =
  { expr = "(- (read))"; optimized = "(- (read))"; value = -12; inputs = [ 12 ]; message = "negate (2)" }

let add_read_and_negate_72 =
  { expr = "(+ (read) (- 72))"; optimized = "(+ -72 (read))"; value = -72; inputs = [ 0 ]; message = "negate (3)" }

let negate_add_99_50 = { expr = "(- (+ 99 50))"; optimized = "-149"; value = -149; inputs = []; message = "negate (4)" }
let negate_negate_5 = { expr = "(- (- 5))"; optimized = "5"; value = 5; inputs = []; message = "negate (5)" }
let negate_list = [ negate_1; negate_read; add_read_and_negate_72; negate_add_99_50; negate_negate_5 ]

(* var *)
let var_1 = { expr = "(let ([x 1]) x)"; optimized = "1"; value = 1; inputs = []; message = "var (1)" }

let var_shadow =
  { expr = "(let ([x (let ([x 2]) x)]) x)"; optimized = "2"; value = 2; inputs = []; message = "var (2)" }

let body_shadow =
  { expr = "(let ([x 1]) (let ([x 2]) x))"; optimized = "2"; value = 2; inputs = []; message = "var (3)" }

let var_name =
  { expr = "(let ([x-1+program (let ([x 2]) x)]) x-1+program)"
  ; optimized = "2"
  ; value = 2
  ; inputs = []
  ; message = "var (4)"
  }

let var_list = [ var_1; var_shadow; body_shadow; var_name ]

(* let *)
let let_read_order =
  { expr = "(let ([x (read)]) (+ x (- (read))))"
  ; optimized = "(let ([x (read)]) (+ x (- (read))))"
  ; value = 40
  ; inputs = [ 42; 2 ]
  ; message = "let (1)"
  }

let let_number_read =
  { expr = "(let ([x (read)]) (+ (+ x x) (- (read))))"
  ; optimized = "(let ([x (read)]) (+ (+ x x) (- (read))))"
  ; value = 0
  ; inputs = [ 5; 10 ]
  ; message = "let (2)"
  }

let let_no_opt =
  { expr = "(let ([x0 (let ([x1 (+ (read) 504)]) (- x1))]) (let ([x2 (- x0)]) (+ x0 x2)))"
  ; optimized = "(let ([x0 (let ([x1 (+ 504 (read))]) (- x1))]) (let ([x2 (- x0)]) (+ x0 x2)))"
  ; value = 0
  ; inputs = [ 1 ]
  ; message = "let (3)"
  }

let let_simple_opt =
  { expr = "(let ([a (read)]) (+ a (- a)))"
  ; optimized = "(let ([a (read)]) (+ a (- a)))"
  ; value = 0
  ; inputs = [ 1 ]
  ; message = "let (4)"
  }

let let_nested_let =
  { expr = "(let ([v0 (let ([v1 2]) (+ v1 3))]) (- (+ v0 (read))))"
  ; optimized = "(+ -5 (- (read)))"
  ; value = -7
  ; inputs = [ 2 ]
  ; message = "let (5)"
  }

let let_double_read =
  { expr = "(let ([x (+ (read) (read))]) (+ 2 x))"
  ; optimized = "(let ([x (+ (read) (read))]) (+ 2 x))"
  ; value = 7
  ; inputs = [ 2; 3 ]
  ; message = "let (6)"
  }

let let_no_opt_read =
  { expr = "(let ([x (+ (read) (read))]) (+ x x))"
  ; optimized = "(let ([x (+ (read) (read))]) (+ x x))"
  ; value = 10
  ; inputs = [ 2; 3 ]
  ; message = "let (7)"
  }

let let_list =
  [ let_read_order; let_number_read; let_no_opt; let_simple_opt; let_nested_let; let_double_read; let_no_opt_read ]

(* random *)
let randp n =
  let ast = Rlang.Lang.randp n in
  let expr = Rlang.Lang.pp ast in
  let optimized = Rlang.Lang.pp (Rlang.Interp.optimize ast) in
  let inputs = Rlang.Generator.generate_input_for_randp ast in
  let value = Rlang.Lang.interp ast ~inputs in
  let message = "randp" ^ string_of_int n ^ ":\nInput: " ^ [%show: int list] inputs ^ "\nExpr:" ^ expr in
  { expr; optimized; value; inputs; message }
