open OUnit2

type rtest =
  { expr : string
  ; optimized : string
  ; value : int
  ; inputs : int list
  ; message : string
  }

let make_rprog (e : string) : Rlang.Ast.rprogram = e |> Lexing.from_string |> Rlang.Lang.parse

let compiler (expr : Rlang.Ast.expr) : Xlang.Ast.xprogram =
  expr |> Rlang.Interp.optimize |> Passes.Uniquify.uniquify |> Passes.Resolve_complex.resolve_complex
  |> Passes.Explicate_control.explicate_control |> Passes.Select_instr.select_instr |> Passes.Assign_homes.assign_homes
  |> Passes.Patch_instructions.patch_instructions

(* *** Testing without optimized pass *** *)

let test_interp (t : rtest) =
  let p = make_rprog t.expr in
  assert_equal t.value (Rlang.Interp.interp ~inputs:t.inputs p.e) ~msg:("interp: " ^ t.message) ~printer:string_of_int;
  assert_equal t.value
    (Xlang.Interp.interp ~inputs:t.inputs (compiler p.e))
    ~msg:("compiler: " ^ t.message) ~printer:string_of_int

let test_compiler (t : rtest) =
  let p = make_rprog t.expr in
  let inputs = List.map string_of_int t.inputs in
  assert_equal (string_of_int t.value)
    (Xlang.Assemble.assemble ~inputs (compiler p.e))
    ~msg:("compiler: " ^ t.message)
    ~printer:(fun x -> x)

(* *** Testing with optimized pass *** *)

let test_optimize (t : rtest) =
  let p_expr = make_rprog t.expr in
  let p_optimized = make_rprog t.optimized in
  assert_equal
    (Rlang.Interp.interp ~inputs:t.inputs p_optimized.e)
    (Rlang.Interp.interp ~inputs:t.inputs p_expr.e)
    ~msg:("optimize vs. non-optimize: " ^ t.message)
    ~printer:string_of_int;
  assert_equal p_optimized.e (Rlang.Interp.optimize p_expr.e)
    ~msg:("optimize AST vs. expr optimize AST: " ^ t.message)
    ~printer:Rlang.Pp.pp
