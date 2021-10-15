open Ast

(* Setup *)
let regs = [ RSP; RBP; RAX; RBX; RCX; RDX; RSI; RDI; R8; R9; R10; R11; R12; R13; R14; R15 ]

let init_ms (lst : (label * block) list) (read_int : unit -> int) : ms =
  { regs = List.combine regs (List.init (List.length regs) (fun _ -> 0))
  ; addrs = []
  ; vars = []
  ; labels = lst
  ; read_int
  }

(* Helpers *)
(* 1/3: get value *)
let rec get_val (ms : ms) (arg : arg) : int =
  match arg with
  | Constant n -> n
  | Reg r -> List.assoc r ms.regs
  | Deref (r, offset) ->
    let addr = get_val ms (Reg r) + offset in
    List.assoc addr ms.addrs
  | Ref v -> List.assoc v ms.vars

(* 2/3: Update ms *)
let update_ms (ms : ms) (arg : arg) (nn : int) : ms =
  match arg with
  | Constant _ -> raise (Failure "'update_ms()' Error - Can't update a number")
  | Reg r ->
    let updated_regs = (r, nn) :: ms.regs in
    { ms with regs = updated_regs }
  | Deref (r, offset) ->
    let addr = get_val ms (Reg r) + offset in
    let updated_addrs = (addr, nn) :: ms.addrs in
    { ms with addrs = updated_addrs }
  | Ref v ->
    let updated_vars = (v, nn) :: ms.vars in
    { ms with vars = updated_vars }

(* 3/3: Arith *)
let arith (ms : ms) (op : int -> int -> int) (src : arg) (dst : arg) : ms =
  let src_val : int = get_val ms src in
  let dst_val : int = get_val ms dst in
  update_ms ms dst (op dst_val src_val)

(* Interp instr *)
let rec interp_ii (ms : ms) (i : instr) (ir : instr list) : ms =
  match i with
  | Addq (src, dst) ->
    let ms' : ms = arith ms ( + ) src dst in
    interp_is ms' ir
  | Subq (src, dst) ->
    let ms' : ms = arith ms ( - ) src dst in
    interp_is ms' ir
  | Movq (src, dst) ->
    let src_val : int = get_val ms src in
    let ms' : ms = update_ms ms dst src_val in
    interp_is ms' ir
  | Retq ->
    let lst : instr list = [ Addq (Constant 8, Reg RSP) ] in
    interp_is ms lst
  | Negq arg ->
    let arg_val : int = get_val ms arg in
    let ms' : ms = update_ms ms arg (-1 * arg_val) in
    interp_is ms' ir
  | Callq "read" ->
    let n = ms.read_int () in
    let ms' : ms = update_ms ms (Reg RAX) n in
    interp_is ms' ir
  | Callq lbl ->
    let lst : instr list = [ Subq (Constant 8, Reg RSP) ] in
    let ms' = interp_is ms lst in
    let ms'' = interp_b ms' lbl in
    interp_is ms'' ir
  | Jmp lbl -> interp_b ms lbl
  | Pushq src ->
    let lst : instr list = [ Subq (Constant 8, Reg RSP); Movq (src, Deref (RSP, 0)) ] in
    interp_is ms (lst @ ir)
  | Popq dst ->
    let lst : instr list = [ Movq (Deref (RSP, 0), dst); Addq (Constant 8, Reg RSP) ] in
    interp_is ms (lst @ ir)

(* Interp instr list *)
and interp_is (ms : ms) (instrs : instr list) : ms =
  match instrs with
  | [] -> ms
  | i :: ir -> interp_ii ms i ir

(* Interp block *)
and interp_b (ms : ms) (label : label) : ms =
  let block = List.assoc label ms.labels in
  interp_is ms block.instrs

(* Interp program *)
let interp_p (read_int : unit -> int) (p : p) : ms =
  let ms0 = init_ms p.blks read_int in
  interp_b ms0 "entry"

(* Get value from RAX *)
let interp ?(inputs = []) (p : p) : int =
  let read_int : unit -> int = Utils.Repl.make_read inputs in
  let ms = interp_p read_int p in
  List.assoc RAX ms.regs
