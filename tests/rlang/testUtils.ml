open OUnit2

type rtest =
  { expr : string
  ; optimized : string
  ; value : int
  ; inputs : int list
  ; message : string
  }

let make_rprog (e : string) : Rlang.rprogram = e |> Lexing.from_string |> Rlang.parse

let compiler (rprog : Rlang.rprogram) : Xlang.Ast.xprogram =
  rprog |> Rlang.optimize |> Passes.Uniquify.uniquify |> Passes.Resolve_complex.resolve_complex
  |> Passes.Explicate_control.explicate_control |> Passes.Select_instr.select_instr |> Passes.Assign_homes.assign_homes
  |> Passes.Patch_instructions.patch_instructions

(* *** Testing without optimized pass *** *)

let test_interp (t : rtest) =
  let rprog = make_rprog t.expr in
  assert_equal t.value (Rlang.interp ~inputs:t.inputs rprog) ~msg:("interp: " ^ t.message) ~printer:string_of_int;
  assert_equal t.value
    (Xlang.Interp.interp ~inputs:t.inputs (compiler rprog))
    ~msg:("compiler: " ^ t.message) ~printer:string_of_int

let test_compiler (t : rtest) =
  let rprog = make_rprog t.expr in
  let inputs = List.map string_of_int t.inputs in
  assert_equal (string_of_int t.value)
    (Xlang.Assemble.assemble ~inputs (compiler rprog))
    ~msg:("compiler: " ^ t.message)
    ~printer:(fun x -> x)

(* *** Testing with optimized pass *** *)

let test_optimize (t : rtest) =
  let rprog_expr = make_rprog t.expr in
  let rprog_optimized = make_rprog t.optimized in
  assert_equal
    (Rlang.interp ~inputs:t.inputs rprog_optimized)
    (Rlang.interp ~inputs:t.inputs rprog_expr)
    ~msg:("optimize vs. non-optimize: " ^ t.message)
    ~printer:string_of_int;
  assert_equal rprog_optimized (Rlang.optimize rprog_expr)
    ~msg:("optimize AST vs. expr optimize AST: " ^ t.message)
    ~printer:Rlang.pp
