open Core
open Compiler
(* open Instruction *)

let parse_program_from_string str =
  Parser.parse_program Lexer.read (Lexing.from_string str)
;;

(* 
let program =
  [ `Label "f"
  ; `Assign ("t0", Plus (Var "a0", Var "a1"))
  ; `Assign ("t1", Plus (Var "a0", Var "a2"))
  ; `Assign ("t2", Int 0)
  ; `Assign ("t3", Int 4)
  ; `Cond (Less_than (Var "t3", Var "a0"), "lab1")
  ; `Assign ("t4", Plus (Var "a0", Var "t1"))
  ; `Label "lab2"
  ; `Jump "lab2"
  ; `Assign ("t2", Plus (Var "a0", Var "a3"))
  ; `Jump "labend"
  ; `Label "lab1"
  ; `Cond (Equal (Var "a0", Int 0), "lab3")
  ; `Assign ("t4", Var "t1")
  ; `Jump "labend"
  ; `Label "lab3"
  ; `Assign ("t4", Plus (Var "a0", Var "a4"))
  ; `Jump "labend"
  ; `Label "labend"
  ; `Assign ("a0", Plus (Var "t4", Var "t2"))
  ; `Ret
  ]
;;

let program2 =
  [ `Label "f"
  ; `Assign ("i", Int 1)
  ; `Assign ("j", Int 1)
  ; `Assign ("k", Int 0)
  ; `Label "loop"
  ; `Cond (Less_than (Var "k", Int 100), "lab1")
  ; `Ret
  ; `Label "lab1"
  ; `Cond (Less_than (Var "j", Int 20), "lab2")
  ; `Assign ("j", Var "k")
  ; `Assign ("k", Plus (Var "k", Int 2))
  ; `Jump "loop_end"
  ; `Label "lab2"
  ; `Assign ("j", Var "i")
  ; `Assign ("k", Plus (Var "k", Int 1))
  ; `Label "loop_end"
  ; `Jump "loop"
  ]
;; *)

let program =
  parse_program_from_string
    {|
      #f:
        i := 1;
        j := 1;
        k := 0;
      #loop:
        if k < 100 then goto #lab1;
        ret;
      #lab1:
        if j < 20 then goto #lab2;
        j := k;
        k := k + 2;
        goto #loopend;
      #lab2:
        j := i;
        k := k + 1;
      #loopend:
        goto #loop;
    |}
;;

let program2 =
  parse_program_from_string
    {|
      #f:
        if x < 10 then goto #lab1;
        a := x + y;
        goto #lab3;
      #lab1:
        b := x + y;
        goto #lab3;
      #lab3:
        c := x + y;
        ret;
    |}
;;

let simple_to_dot simple =
  Ir.Simple.(
    to_dot ~label:(fun block -> Node.sexp_of_t block |> Sexp.to_string_hum) simple)
;;

let basic_block_to_dot basic_block =
  Ir.Basic_block.(
    to_dot ~label:(fun block -> Node.sexp_of_t block |> Sexp.to_string_hum) basic_block)
;;

let ssa_to_dot ssa =
  Ir.Ssa.(to_dot ~label:(fun block -> Node.sexp_of_t block |> Sexp.to_string_hum) ssa)
;;

let output_dot ~to_dot ~name flowgraph =
  Out_channel.write_all name ~data:(to_dot flowgraph)
;;

let () =
  let simple = Ir.Simple.of_program program2 in
  output_dot ~to_dot:simple_to_dot ~name:"./simple.dot" simple;
  (* let csed = Dataflow.Transform.common_subexpr_elim simple in *)
  let avail = Dataflow.Avail.analysis simple in
  output_dot
    ~to_dot:(fun ir -> Dataflow.to_dot ir avail ~sexp_of_a:Instruction.sexp_of_expression)
    ~name:"./avail.dot"
    simple;
  let live = Dataflow.Liveness.analysis simple in
  output_dot
    ~to_dot:(fun ir -> Dataflow.to_dot ir live ~sexp_of_a:sexp_of_string)
    ~name:"./live.dot"
    simple;
  let cse = Dataflow.Transform.common_subexpr_elim simple in
  output_dot ~to_dot:simple_to_dot ~name:"./cse.dot" cse
;;
(* let basic_block = Ir.Basic_block.of_program program in
  output_dot ~to_dot:basic_block_to_dot ~name:"./basic_block.dot" basic_block;
  let ssa = Ir.Ssa.of_basic_block basic_block in
  output_dot ~to_dot:ssa_to_dot ~name:"./ssa.dot" ssa *)
