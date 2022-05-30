open Js_of_ocaml
open Core
open Compiler
open Compiler.Instruction

let rec stringify_expr expr =
  match expr with
  | Var x -> x
  | Int x -> string_of_int x
  | Plus (expr1, expr2) -> [%string "(%{stringify_expr expr1} + %{stringify_expr expr2})"]
;;

let stringify_cond cond =
  match cond with
  | Equal (expr1, expr2) -> [%string "%{stringify_expr expr1} = %{stringify_expr expr2}"]
  | Greater_than (expr1, expr2) ->
    [%string "%{stringify_expr expr1} > %{stringify_expr expr2}"]
  | Less_than (expr1, expr2) ->
    [%string "%{stringify_expr expr1} < %{stringify_expr expr2}"]
  | Not_equal (expr1, expr2) ->
    [%string "%{stringify_expr expr1} != %{stringify_expr expr2}"]
;;

let stringify_instr instr =
  match instr with
  | `Call (lab, exprs) ->
    let exprs = String.concat ~sep:", " (List.map exprs ~f:stringify_expr) in
    [%string "%{lab}(%{exprs});"]
  | `Calli (x, exprs) ->
    let exprs = String.concat ~sep:", " (List.map exprs ~f:stringify_expr) in
    [%string "[%{x}](%{exprs});"]
  | `Jump lab -> [%string "goto %{lab};"]
  | `Jump_indir x -> [%string "goto [%{x}];"]
  | `Cond (cond, lab) -> [%string "if %{stringify_cond cond} goto %{lab};"]
  | `Ret -> "ret;"
  | `Assign (x, expr) -> [%string "%{x} := %{stringify_expr expr};"]
  | `Address_of (x, rhs) ->
    (match rhs with
    | `Label lab -> [%string "%{x} := &%{lab};"]
    | `Var x -> [%string "%{x} := &{x};"])
  | `Deref (x, expr) -> [%string "%{x} := [%{stringify_expr expr}];"]
  | `Store (expr, x) -> [%string "[%{x}] := %{stringify_expr expr};"]
  | `Label lab -> [%string "%{lab}:"]
;;

let stringify_block block = String.concat ~sep:"\n" (List.map block ~f:stringify_instr)

let stringify_phi (`Phi (x, vars)) =
  let vars = String.concat ~sep:", " vars in
  [%string "%{x} := Φ(%{vars})"]
;;

let stringify_set s ~f = "{" ^ String.concat ~sep:", " (List.map (Set.to_list s) ~f) ^ "}"
let stringify_expr_set exprs = stringify_set exprs ~f:stringify_expr

let parse_program_from_string str =
  Parser.parse_program Lexer.read (Lexing.from_string str)
;;

let simple_to_dot simple =
  Ir.Simple.(to_dot ~label:(fun block -> stringify_instr block.instr) simple)
;;

let basic_block_to_dot basic_block =
  Ir.Basic_block.(to_dot ~label:(fun block -> stringify_block block.block) basic_block)
;;

let ssa_to_dot ssa =
  Ir.Ssa.(
    to_dot
      ~label:(fun block ->
        String.concat
          ~sep:"\n"
          (List.map block.phis ~f:stringify_phi @ [ stringify_block block.block ]))
      ssa)
;;

let _ =
  Js.export
    "optimizer"
    (object%js
       method simpleFromString str =
         let program = parse_program_from_string (Js.to_string str) in
         let ir = Ir.Simple.of_program program in
         Js.string (simple_to_dot ir)

       method simpleAvailFromString str =
         let program = parse_program_from_string (Js.to_string str) in
         let ir = Ir.Simple.of_program program in
         let avail = Dataflow.Avail.analysis ir in
         let dot =
           Ir.Simple.to_dot
             ~label:(fun block ->
               let instr = stringify_instr block.instr in
               let avail_node = avail block in
               let in_ = stringify_expr_set avail_node.in_ in
               let out = stringify_expr_set avail_node.out in
               [%string "AVAIL_IN = %{in_}\n%{instr}\nAVAIL_OUT = %{out}"])
             ir
         in
         Js.string dot

       method simpleLiveFromString str =
         let program = parse_program_from_string (Js.to_string str) in
         let ir = Ir.Simple.of_program program in
         let live = Dataflow.Liveness.analysis ir in
         let dot =
           Ir.Simple.to_dot
             ~label:(fun block ->
               let instr = stringify_instr block.instr in
               let live_node = live block in
               let in_ = stringify_set live_node.in_ ~f:(fun x -> x) in
               let out = stringify_set live_node.out ~f:(fun x -> x) in
               [%string "LIVE_IN = %{in_}\n%{instr}\nLIVE_OUT = %{out}"])
             ir
         in
         Js.string dot

       method basicBlockFromString str =
         let program = parse_program_from_string (Js.to_string str) in
         let ir = Ir.Basic_block.of_program program in
         Js.string (basic_block_to_dot ir)

       method ssaFromString str =
         let program = parse_program_from_string (Js.to_string str) in
         let ir = Ir.Basic_block.of_program program in
         let ir = Ir.Ssa.of_basic_block ir in
         Js.string (ssa_to_dot ir)
    end)
;;