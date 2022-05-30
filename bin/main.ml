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

let exam =
  parse_program_from_string
    {|
      #foo:
        t0 := 1000;
        t1 := 0;
        t2 := 0;
        t3 := arg0 * 2;
        t4 := arg1 * 2;
        t5 := &#lab4;
        [t0] := t5;
        goto #lab7;
      #lab1:
        t6 := 1;
        if t1 = arg2 then goto #lab3;
        t7 := t4 * t4;
        if t1 = t7 then goto #lab5;
        goto #lab6;
      #lab2:
        t8 := &#lab5;
        [t0] := t8;
        goto #lab5;
      #lab3:
        t9 := t4 * t4;
        t2 := t2 + t9;
        t6 := t6 + 1;
        t10 := [t0];
        goto [t10];
      #lab4:
        t2 := t2 + t1;
        goto #lab6;
      #lab5:
        t2 := t2 + t6;
      #lab6:
        t1 := t1 + 1;
      #lab7:
        if t1 != t3 then goto #lab1;
        res0 := t2 * 4;
        t11 := &#lab1;
        [t0] := t11;
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
  let simple = Ir.Simple.of_program exam in
  output_dot ~to_dot:simple_to_dot ~name:"./simple.dot" simple;
  let basic_block = Ir.Basic_block.of_program exam in
  output_dot ~to_dot:basic_block_to_dot ~name:"./basic_block.dot" basic_block;
  (* let csed = Dataflow.Transform.common_subexpr_elim simple in *)
  (* let optimized = Transform.single Transform.common_subexpr_elim simple in
  output_dot ~to_dot:simple_to_dot ~name:"./opt.dot" optimized; *)
  let optimized = Transform.aggressive simple in
  output_dot ~to_dot:simple_to_dot ~name:"./opt2.dot" optimized
;;
(* let basic_block = Ir.Basic_block.of_program program in
  output_dot ~to_dot:basic_block_to_dot ~name:"./basic_block.dot" basic_block;
  let ssa = Ir.Ssa.of_basic_block basic_block in
  output_dot ~to_dot:ssa_to_dot ~name:"./ssa.dot" ssa *)
