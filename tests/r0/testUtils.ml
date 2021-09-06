open OUnit2

type test =
  { ast : R0.Ast.r0
  ; optimized : R0.Ast.r0
  ; interp : int
  ; input : int list
  ; message : string
  }

let test_interp (t : test) =
  assert_equal t.interp
    (R0.Interp.interp t.ast ~input:(Repl.make_read t.input))
    ~msg:("interp: " ^ t.message) ~printer:string_of_int

let test_optimize (t : test) =
  let input = Repl.make_read t.input in
  assert_equal (R0.Interp.interp t.optimized ~input) (R0.Interp.interp t.ast ~input) ~msg:("optimize: " ^ t.message)
    ~printer:string_of_int
