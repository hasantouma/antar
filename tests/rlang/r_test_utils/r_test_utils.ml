type pass_t =
  { uniquify : R1.Ast.expr (* uniquify pass *)
  ; is_uniquify : bool
  ; resolve_complex : R1.Ast.expr (* resolve_complex pass *)
  ; is_resolve_complex : bool
  ; explicate_control : C0.Ast.p (* explicate_control pass *)
  ; optimize : R1.Ast.expr (* optimize pass *)
  ; opt_uniquify : R1.Ast.expr (* opt_uniquify pass *)
  ; opt_is_uniquify : bool
  ; opt_resolve_complex : R1.Ast.expr (* opt_resolve_complex pass *)
  ; opt_is_resolve_complex : bool
  ; opt_explicate_control : C0.Ast.p (* opt_explicate_control pass *)
  }

(* Calculating ASTs for each pass *)
let r1_to_passes (expr : R1.Ast.expr) : pass_t =
  (* *** Testing without optimize pass *** *)

  (* uniquify pass *)
  let uniquify : R1.Ast.expr = Passes.Uniquify.uniquify expr in
  let is_uniquify : bool = uniquify |> Passes.Uniquify.is_uniquify in

  (* resolve_complex pass *)
  let resolve_complex : R1.Ast.expr = uniquify |> Passes.Resolve_complex.resolve_complex in
  let is_resolve_complex : bool = resolve_complex |> Passes.Resolve_complex.is_resolve_complex in

  (* explicate_control pass *)
  let explicate_control : C0.Ast.p = resolve_complex |> Passes.Explicate_control.explicate_control in

  (* *** Testing with optimize pass *** *)

  (* optimize pass *)
  let optimize : R1.Ast.expr = R1.Interp.optimize expr in

  (* opt_uniquify pass *)
  let opt_uniquify : R1.Ast.expr = optimize |> Passes.Uniquify.uniquify in
  let opt_is_uniquify : bool = opt_uniquify |> Passes.Uniquify.is_uniquify in

  (* opt_resolve_complex pass *)
  let opt_resolve_complex : R1.Ast.expr = opt_uniquify |> Passes.Resolve_complex.resolve_complex in
  let opt_is_resolve_complex : bool = opt_resolve_complex |> Passes.Resolve_complex.is_resolve_complex in

  (* opt_explicate_control pass *)
  let opt_explicate_control : C0.Ast.p = opt_resolve_complex |> Passes.Explicate_control.explicate_control in

  { uniquify
  ; is_uniquify
  ; resolve_complex
  ; is_resolve_complex
  ; explicate_control
  ; optimize (* optimized passes *)
  ; opt_uniquify
  ; opt_is_uniquify
  ; opt_resolve_complex
  ; opt_is_resolve_complex
  ; opt_explicate_control
  }
