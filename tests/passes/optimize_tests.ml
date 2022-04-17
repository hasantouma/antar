open OUnit2
open TestUtils
open Passes

let make_rprog (e : string) : Rlang.rprogram = e |> Lexing.from_string |> Rlang.parse

(* int *)
let opt1 = make_rprog "1"
let opt1' = make_rprog "1"
let opt2 = make_rprog "42"
let opt2' = make_rprog "42"

(* read *)
let opt3 = make_rprog "(read)"
let opt3' = opt3

(* add *)
let opt4 = make_rprog "(+ 1 2)"
let opt4' = make_rprog "3"
let opt5 = make_rprog "(+ 33 (read))"
let opt5' = opt5
let opt6 = make_rprog "(+ (read) 72)"
let opt6' = make_rprog "(+ 72 (read))"
let opt7 = make_rprog "(+ (read) (read))"
let opt7' = opt7

(* negate *)
let opt8 = make_rprog "(- 1)"
let opt8' = make_rprog "-1"
let opt9 = make_rprog "(- (read))"
let opt9' = opt9
let opt10 = make_rprog "(+ (read) (- 72))"
let opt10' = make_rprog "(+ -72 (read))"
let opt11 = make_rprog "(- (+ 99 50))"
let opt11' = make_rprog "-149"
let opt12 = make_rprog "(- (- 5))"
let opt12' = make_rprog "5"
let opt13 = make_rprog "(- (- (- 5)))"
let opt13' = make_rprog "-5"

(* var *)
let opt14 = make_rprog "(let ([x 1]) x)"
let opt14' = make_rprog "1"
let opt15 = make_rprog "(let ([x (let ([x 2]) x)]) x)"
let opt15' = make_rprog "2"
let opt16 = make_rprog "(let ([x 1]) (let ([x 2]) x))"
let opt16' = make_rprog "2"
let opt17 = make_rprog "(let ([x-1+program (let ([x 2]) x)]) x-1+program)"
let opt17' = make_rprog "2"

(* let *)
let opt18 = make_rprog "(let ([x (read)]) (+ x (- (read))))"
let opt18' = make_rprog "(let ([x (read)]) (+ x (- (read))))"
let opt19 = make_rprog "(let ([x (read)]) (+ (+ x x) (- (read))))"
let opt19' = make_rprog "(let ([x (read)]) (+ (+ x x) (- (read))))"
let opt20 = make_rprog "(let ([x0 (let ([x1 (+ (read) 504)]) (- x1))]) (let ([x2 (- x0)]) (+ x0 x2)))"
let opt20' = make_rprog "(let ([x0 (let ([x1 (+ 504 (read))]) (- x1))]) (let ([x2 (- x0)]) (+ x0 x2)))"
let opt21 = make_rprog "(let ([a (read)]) (+ a (- a)))"
let opt21' = make_rprog "(let ([a (read)]) (+ a (- a)))"
let opt22 = make_rprog "(let ([v0 (let ([v1 2]) (+ v1 3))]) (- (+ v0 (read))))"
let opt22' = make_rprog "(+ -5 (- (read)))"

let test_optimize ctxt =
  assert_equal opt1' (optimize opt1) ~cmp:rlang_alpha_equiv ~msg:"optimize: opt1" ~printer:Rlang.pp;
  assert_equal opt2' (optimize opt2) ~cmp:rlang_alpha_equiv ~msg:"optimize: opt2" ~printer:Rlang.pp;
  assert_equal opt3' (optimize opt3) ~cmp:rlang_alpha_equiv ~msg:"optimize: opt3" ~printer:Rlang.pp;
  assert_equal opt4' (optimize opt4) ~cmp:rlang_alpha_equiv ~msg:"optimize: opt4" ~printer:Rlang.pp;
  assert_equal opt5' (optimize opt5) ~cmp:rlang_alpha_equiv ~msg:"optimize: opt5" ~printer:Rlang.pp;
  assert_equal opt6' (optimize opt6) ~cmp:rlang_alpha_equiv ~msg:"optimize: opt6" ~printer:Rlang.pp;
  assert_equal opt7' (optimize opt7) ~cmp:rlang_alpha_equiv ~msg:"optimize: opt7" ~printer:Rlang.pp;
  assert_equal opt8' (optimize opt8) ~cmp:rlang_alpha_equiv ~msg:"optimize: opt8" ~printer:Rlang.pp;
  assert_equal opt9' (optimize opt9) ~cmp:rlang_alpha_equiv ~msg:"optimize: opt9" ~printer:Rlang.pp;
  assert_equal opt10' (optimize opt10) ~cmp:rlang_alpha_equiv ~msg:"optimize: opt10" ~printer:Rlang.pp;
  assert_equal opt11' (optimize opt11) ~cmp:rlang_alpha_equiv ~msg:"optimize: opt11" ~printer:Rlang.pp;
  assert_equal opt12' (optimize opt12) ~cmp:rlang_alpha_equiv ~msg:"optimize: opt12" ~printer:Rlang.pp;
  assert_equal opt13' (optimize opt13) ~cmp:rlang_alpha_equiv ~msg:"optimize: opt13" ~printer:Rlang.pp;
  assert_equal opt14' (optimize opt14) ~cmp:rlang_alpha_equiv ~msg:"optimize: opt14" ~printer:Rlang.pp;
  assert_equal opt15' (optimize opt15) ~cmp:rlang_alpha_equiv ~msg:"optimize: opt15" ~printer:Rlang.pp;
  assert_equal opt16' (optimize opt16) ~cmp:rlang_alpha_equiv ~msg:"optimize: opt16" ~printer:Rlang.pp;
  assert_equal opt17' (optimize opt17) ~cmp:rlang_alpha_equiv ~msg:"optimize: opt17" ~printer:Rlang.pp;
  assert_equal opt18' (optimize opt18) ~cmp:rlang_alpha_equiv ~msg:"optimize: opt18" ~printer:Rlang.pp;
  assert_equal opt19' (optimize opt19) ~cmp:rlang_alpha_equiv ~msg:"optimize: opt19" ~printer:Rlang.pp;
  assert_equal opt20' (optimize opt20) ~cmp:rlang_alpha_equiv ~msg:"optimize: opt20" ~printer:Rlang.pp;
  assert_equal opt21' (optimize opt21) ~cmp:rlang_alpha_equiv ~msg:"optimize: opt21" ~printer:Rlang.pp;
  assert_equal opt22' (optimize opt22) ~cmp:rlang_alpha_equiv ~msg:"optimize: opt22" ~printer:Rlang.pp

let test_interp_optimize ctxt =
  assert_equal (Rlang.interp opt1') (Rlang.interp opt1) ~msg:"interp_optimize: opt1" ~printer:string_of_int;
  assert_equal (Rlang.interp opt2') (Rlang.interp opt2) ~msg:"interp_optimize: opt2" ~printer:string_of_int;
  assert_equal (Rlang.interp ~inputs:[ 12 ] opt3') (Rlang.interp ~inputs:[ 12 ] opt3) ~msg:"interp_optimize: opt3"
    ~printer:string_of_int;
  assert_equal (Rlang.interp opt4') (Rlang.interp opt4) ~msg:"interp_optimize: opt4" ~printer:string_of_int;
  assert_equal (Rlang.interp ~inputs:[ -3 ] opt5') (Rlang.interp ~inputs:[ -3 ] opt5) ~msg:"interp_optimize: opt5"
    ~printer:string_of_int;
  assert_equal (Rlang.interp ~inputs:[ 8 ] opt6') (Rlang.interp ~inputs:[ 8 ] opt6) ~msg:"interp_optimize: opt6"
    ~printer:string_of_int;
  assert_equal
    (Rlang.interp ~inputs:[ 2; 3 ] opt7')
    (Rlang.interp ~inputs:[ 2; 3 ] opt7)
    ~msg:"interp_optimize: opt7" ~printer:string_of_int;
  assert_equal (Rlang.interp opt8') (Rlang.interp opt8) ~msg:"interp_optimize: opt8" ~printer:string_of_int;
  assert_equal (Rlang.interp ~inputs:[ 12 ] opt9') (Rlang.interp ~inputs:[ 12 ] opt9) ~msg:"interp_optimize: opt9"
    ~printer:string_of_int;
  assert_equal (Rlang.interp ~inputs:[ 15 ] opt10') (Rlang.interp ~inputs:[ 15 ] opt10) ~msg:"interp_optimize: opt10"
    ~printer:string_of_int;
  assert_equal (Rlang.interp opt11') (Rlang.interp opt11) ~msg:"interp_optimize: opt11" ~printer:string_of_int;
  assert_equal (Rlang.interp opt12') (Rlang.interp opt12) ~msg:"interp_optimize: opt12" ~printer:string_of_int;
  assert_equal (Rlang.interp opt13') (Rlang.interp opt13) ~msg:"interp_optimize: opt13" ~printer:string_of_int;
  assert_equal (Rlang.interp opt14') (Rlang.interp opt14) ~msg:"interp_optimize: opt14" ~printer:string_of_int;
  assert_equal (Rlang.interp opt15') (Rlang.interp opt15) ~msg:"interp_optimize: opt15" ~printer:string_of_int;
  assert_equal (Rlang.interp opt16') (Rlang.interp opt16) ~msg:"interp_optimize: opt16" ~printer:string_of_int;
  assert_equal (Rlang.interp opt17') (Rlang.interp opt17) ~msg:"interp_optimize: opt17" ~printer:string_of_int;
  assert_equal
    (Rlang.interp ~inputs:[ 42; 2 ] opt18')
    (Rlang.interp ~inputs:[ 42; 2 ] opt18)
    ~msg:"interp_optimize: opt18" ~printer:string_of_int;
  assert_equal
    (Rlang.interp ~inputs:[ 5; 10 ] opt19')
    (Rlang.interp ~inputs:[ 5; 10 ] opt19)
    ~msg:"interp_optimize: opt19" ~printer:string_of_int;
  assert_equal (Rlang.interp ~inputs:[ 1 ] opt20') (Rlang.interp ~inputs:[ 1 ] opt20) ~msg:"interp_optimize: opt20"
    ~printer:string_of_int;
  assert_equal (Rlang.interp ~inputs:[ 1 ] opt21') (Rlang.interp ~inputs:[ 1 ] opt21) ~msg:"interp_optimize: opt21"
    ~printer:string_of_int;
  assert_equal (Rlang.interp ~inputs:[ 2 ] opt22') (Rlang.interp ~inputs:[ 2 ] opt22) ~msg:"interp_optimize: opt22"
    ~printer:string_of_int

let suite = "optimize_tests" >::: [ "optimize" >:: test_optimize; "interp_optimize" >:: test_interp_optimize ]
let _ = run_test_tt_main suite
