open TestUtils

(* int *)
let one = { expr = "1"; optimized = "1"; interp = 1; inputs = []; message = "int (1)" }

let forty_two = { expr = "42"; optimized = "42"; interp = 42; inputs = []; message = "int (2)" }

let int_list = [ one; forty_two ]

(* read *)
let read = { expr = "(read)"; optimized = "(read)"; interp = 23; inputs = [ 23 ]; message = "read (1)" }

let read_list = [ read ]

(* add *)
let add_1_2 = { expr = "(+ 1 2)"; optimized = "3"; interp = 3; inputs = []; message = "add (1)" }

let add_33_read =
  { expr = "(+ 33 (read))"; optimized = "(+ 33 (read))"; interp = 36; inputs = [ 3 ]; message = "add (2)" }

let add_read_72 =
  { expr = "(+ (read) 72)"; optimized = "(+ 72 (read))"; interp = 80; inputs = [ 8 ]; message = "add (3)" }

let add_read_read =
  { expr = "(+ (read) (read))"; optimized = "(+ (read) (read))"; interp = 5; inputs = [ 2; 3 ]; message = "add (4)" }

let add_list = [ add_1_2; add_33_read; add_read_72; add_read_read ]

(* negate *)
let negate_1 = { expr = "(- 1)"; optimized = "(- 1)"; interp = -1; inputs = []; message = "negate (1)" }

let negate_read =
  { expr = "(- (read))"; optimized = "(- (read))"; interp = -12; inputs = [ 12 ]; message = "negate (2)" }

let add_read_and_negate_72 =
  { expr = "(+ (read) (- 72))"; optimized = "(+ -72 (read))"; interp = -72; inputs = [ 0 ]; message = "negate (3)" }

let negate_add_99_50 =
  { expr = "(- (+ 99 50))"; optimized = "-149"; interp = -149; inputs = []; message = "negate (4)" }

let negate_negate_5 = { expr = "(- (- 5))"; optimized = "5"; interp = 5; inputs = []; message = "negate (5)" }

let negate_list = [ negate_1; negate_read; add_read_and_negate_72; negate_add_99_50; negate_negate_5 ]

(* random *)
let randp n =
  let ast = R0.Generator.randp n in
  let expr = R0.Pp.pp ast in
  let optimized = R0.Pp.pp (R0.Interp.optimize ast) in
  let inputs = R0.Generator.generate_input_for_randp ast in
  let interp = R0.Interp.interp ast ~inputs in
  let message = "randp" ^ string_of_int n ^ ":\nInput: " ^ pp_list inputs ^ "\nExpr:" ^ expr in
  { expr; optimized; interp; inputs; message }
