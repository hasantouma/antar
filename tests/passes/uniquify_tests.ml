open OUnit2
open Passes.Uniquify

let alpha_equiv e1 e2 : bool =
  let rec go env e1 e2 =
    match (e1, e2) with
    | `EInt n, `EInt m -> n = m
    | `ERead, `ERead -> true
    | `EVar x, `EVar y -> (
      let y_opt = List.assoc_opt x env in
      match y_opt with
      | Some y' -> y = y'
      | None -> x = y)
    | `ENegate e1', `ENegate e2' -> go env e1' e2'
    | `EAdd (l1, r1), `EAdd (l2, r2) -> go env l1 l2 && go env r1 r2
    | `ELet (x, xe, be1), `ELet (y, ye, be2) ->
      let env' = (x, y) :: env in
      go env xe ye && go env' be1 be2
    | _ -> false
  in
  go [] e1 e2

let u1 = `ELet ("a", `ERead, `EVar "a")

let u1' = `ELet ("x0", `ERead, `EVar "x0")

let u2 = `ELet ("a", `ELet ("a", `ENegate (`EInt 3), `EAdd (`EVar "a", `EVar "a")), `EVar "a")

let u2' = `ELet ("x0", `ELet ("x1", `ENegate (`EInt 3), `EAdd (`EVar "x1", `EVar "x1")), `EVar "x0")

let u3 = `ELet ("a", `EInt 4, `ELet ("a", `ENegate (`EInt 3), `EAdd (`EVar "a", `EVar "a")))

let u3' = `ELet ("x0", `EInt 4, `ELet ("x1", `ENegate (`EInt 3), `EAdd (`EVar "x1", `EVar "x1")))

let u4 =
  `EAdd
    ( `ELet ("x", `EInt 7, `EVar "x")
    , `ELet ("x", `EInt 8, `ELet ("x", `EAdd (`EInt 1, `EVar "x"), `EAdd (`EVar "x", `EVar "x"))) )

let u4' =
  `EAdd
    ( `ELet ("x0", `EInt 7, `EVar "x0")
    , `ELet ("x1", `EInt 8, `ELet ("x2", `EAdd (`EInt 1, `EVar "x1"), `EAdd (`EVar "x2", `EVar "x2"))) )

let test_uniquify _ctxt =
  assert_equal u1' (uniquify u1) ~cmp:alpha_equiv ~msg:"u1" ~printer:R1.Pp.pp;
  assert_equal u2' (uniquify u2) ~cmp:alpha_equiv ~msg:"u2" ~printer:R1.Pp.pp;
  assert_equal u3' (uniquify u3) ~cmp:alpha_equiv ~msg:"u3" ~printer:R1.Pp.pp;
  assert_equal u4' (uniquify u4) ~cmp:alpha_equiv ~msg:"u4" ~printer:R1.Pp.pp

let suite = "passes_tests" >::: [ "uniquify" >:: test_uniquify ]

let _ = run_test_tt_main suite
