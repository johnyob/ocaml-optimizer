open Js_of_ocaml
open Core
open Compiler
open Compiler.Instruction

let stringify_block block =
  String.concat ~sep:"\n" (List.map block ~f:Instruction.to_string)
;;

let stringify_phi (`Phi (x, vars)) =
  let vars = String.concat ~sep:", " vars in
  [%string "%{x} := Φ(%{vars})"]
;;

let stringify_set s ~f = "{" ^ String.concat ~sep:", " (List.map (Set.to_list s) ~f) ^ "}"
let stringify_expr_set exprs = stringify_set exprs ~f:Expr.to_string

let parse_program_from_string str =
  Parser.parse_program Lexer.read (Lexing.from_string str)
;;

let simple_to_dot simple =
  Ir.Simple.(to_dot ~label:(fun block -> Instruction.to_string block.instr) simple)
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

let () =
  Js.export
    "optimizer"
    (object%js
       method simpleOptimizedFromString str =
         let program = parse_program_from_string (Js.to_string str) in
         let ir = Ir.Simple.of_program program in
         let optimized_ir = Transform.aggressive ir in
         Js.string (simple_to_dot optimized_ir)

       method simpleFromString str =
         let program = parse_program_from_string (Js.to_string str) in
         let ir = Ir.Simple.of_program program in
         Js.string (simple_to_dot ir)

       method simpleAvailFromString str =
         let program = parse_program_from_string (Js.to_string str) in
         let ir = Ir.Simple.of_program program in
         let avail = Analysis.Avail.analysis ir in
         let dot =
           Ir.Simple.to_dot
             ~label:(fun block ->
               let instr = Instruction.to_string block.instr in
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
         let live = Analysis.Liveness.analysis ir in
         let dot =
           Ir.Simple.to_dot
             ~label:(fun block ->
               let instr = Instruction.to_string block.instr in
               let live_node = live block in
               let in_ = stringify_set live_node.in_ ~f:(fun x -> x) in
               let out = stringify_set live_node.out ~f:(fun x -> x) in
               [%string "LIVE_IN = %{in_}\n%{instr}\nLIVE_OUT = %{out}"])
             ir
         in
         Js.string dot

       method simpleReachFromString str =
         let program = parse_program_from_string (Js.to_string str) in
         let ir = Ir.Simple.of_program program in
         let reach = Analysis.Reach.analysis ir in
         let dot =
           Ir.Simple.to_dot
             ~label:(fun block ->
               let instr = Instruction.to_string block.instr in
               let id node = node.Ir.Simple.Node.id in
               let reach_node = reach block in
               let in_ =
                 stringify_set reach_node.in_ ~f:(fun x -> id x |> Int.to_string)
               in
               let out =
                 stringify_set reach_node.out ~f:(fun x -> id x |> Int.to_string)
               in
               let node_id = id block in
               [%string
                 "REACH_IN = %{in_}\n%{node_id#Int} | %{instr}\nREACH_OUT = %{out}"])
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