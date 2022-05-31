open Core

let fresh_id = Ir.fresh_id

module Ir = Ir.Simple
open Ir
open Analysis
open Instruction

type t = Ir.t -> continue:(unit -> unit) -> Ir.t

let fix ~f : t =
 fun ir ~continue ->
  let is_done = ref false in
  let continue () =
    continue ();
    is_done := false
  in
  let ir = ref ir in
  while not !is_done do
    is_done := true;
    ir := f !ir ~continue
  done;
  !ir
;;

let unreachable_elim =
  fix ~f:(fun ir ~continue ->
      (* Compute the unreachable nodes in the flowgraph *)
      let unreachable_nodes = unreachable_nodes ir in
      (* Remove unreachable nodes *)
      let flowgraph =
        if not (List.is_empty unreachable_nodes) then continue ();
        List.fold_left unreachable_nodes ~init:ir.flowgraph ~f:(fun flowgraph node ->
            print_endline [%string "Removing unreachable node %{Instruction.to_string node.instr} (%{node.id#Int})"];
            Flowgraph.remove_node flowgraph node)
      in
      { ir with flowgraph })
;;

let dead_code_elim =
  fix ~f:(fun ({ entry; _ } as ir) ~continue ->
      print_endline "doing dead_code_elim";
      let live = Liveness.analysis ir in
      let is_dead (node : Node.t) =
        let def = Instruction.def node.instr in
        let out_live = (live node).out in
        Set.length def > 0 && Set.for_all def ~f:(fun x -> not (Set.mem out_live x))
      in
      Ir.Transform.filter_map ir ~f:(fun node ->
          if Node.(node = entry)
          then Some node
          else if is_dead node
          then (
            print_endline [%string "Remove dead instruction %{Instruction.to_string node.instr} (%{node.id#Int})"];
            continue ();
            None)
          else Some node))
;;

let reaching_exprs flowgraph node ~expr =
  let visited = Hash_set.create (module Node) in
  (* [node] is initially visited *)
  Hash_set.add visited node;
  let reaching_exprs = Hash_set.create (module Node) in
  let rec loop node =
    if not (Hash_set.mem visited node)
    then (
      (* print_endline ("visiting " ^ Int.to_string node.id); *)
      (* visit *)
      Hash_set.add visited node;
      (* [expr] not invalidated *)
      if Set.are_disjoint (Expr.free_vars expr) (Instruction.def node.instr)
      then
        (* [node] computes [expr] *)
        if Expr.(
             Set.mem
               (Set.union_list (List.map (Instruction.exprs node.instr) ~f:sub_exprs))
               expr)
        then Hash_set.add reaching_exprs node
        else Flowgraph.pred flowgraph node |> List.iter ~f:loop)
  in
  (* traverse from predecessors *)
  List.iter (Flowgraph.pred flowgraph node) ~f:loop;
  Hash_set.to_list reaching_exprs
;;

let rec replace_expr expr' ~var ~expr =
  let open Expr in
  if expr = expr'
  then Var var
  else (
    match expr with
    | Plus (expr1, expr2) ->
      Plus (replace_expr expr1 ~var ~expr, replace_expr expr2 ~var ~expr)
    | Mul (expr1, expr2) ->
      Mul (replace_expr expr1 ~var ~expr, replace_expr expr2 ~var ~expr)
    | Sub (expr1, expr2) ->
      Sub (replace_expr expr1 ~var ~expr, replace_expr expr2 ~var ~expr)
    | Var _ | Int _ -> expr)
;;

let replace_cond cond ~var ~expr =
  let open Condition in
  match cond with
  | Equal (expr1, expr2) ->
    Equal (replace_expr expr1 ~var ~expr, replace_expr expr2 ~var ~expr)
  | Not_equal (expr1, expr2) ->
    Not_equal (replace_expr expr1 ~var ~expr, replace_expr expr2 ~var ~expr)
  | Less_than (expr1, expr2) ->
    Less_than (replace_expr expr1 ~var ~expr, replace_expr expr2 ~var ~expr)
  | Greater_than (expr1, expr2) ->
    Greater_than (replace_expr expr1 ~var ~expr, replace_expr expr2 ~var ~expr)
;;

let replace_instr instr ~var ~expr =
  match instr with
  | `Assign (x, expr') -> `Assign (x, replace_expr expr' ~var ~expr)
  | `Deref (x, expr') -> `Deref (x, replace_expr expr' ~var ~expr)
  | `Store (expr', x) -> `Store (replace_expr expr' ~var ~expr, x)
  | `Call (label, exprs) -> `Call (label, List.map exprs ~f:(replace_expr ~var ~expr))
  | `Calli (x, exprs) -> `Calli (x, List.map exprs ~f:(replace_expr ~var ~expr))
  | `Cond (cond, label) -> `Cond (replace_cond cond ~var ~expr, label)
  | instr -> instr
;;

let common_subexpr_elim =
  fix ~f:(fun ({ flowgraph; entry } as ir) ~continue ->
      print_endline "doing cse";
      let avail = Avail.analysis ir in
      let flowgraph =
        Flowgraph.fold flowgraph ~init:flowgraph ~f:(fun node flowgraph ->
            match avail node with
            | avail ->
              let sub_exprs =
                (* Compute available subexpressions \cap sub expressions of the node
                 (In accessending order of size) *)
                node.instr
                |> Instruction.exprs
                |> List.map ~f:Expr.sub_exprs
                |> Expr.Set.union_list
                |> Set.inter avail.in_
                |> Set.to_list
                |> List.sort
                     ~compare:
                       Expr.(fun expr1 expr2 -> -Int.compare (size expr1) (size expr2))
              in
              List.fold_left sub_exprs ~init:flowgraph ~f:(fun flowgraph expr ->
                  continue ();
                  print_endline
                    [%string "Eliminating common subexpression %{Expr.to_string expr} (%{node.id#Int})"];
                  (* Find reaching exprs of [expr] *)
                  let reaching_exprs = reaching_exprs flowgraph node ~expr in
                  (* Generate new variable *)
                  let var = Var.fresh ~prefix:"t#" () in
                  (* Transform reaching expressions *)
                  let flowgraph =
                    List.fold_left
                      reaching_exprs
                      ~init:flowgraph
                      ~f:(fun flowgraph node ->
                        Flowgraph.update_many
                          flowgraph
                          ~from:node
                          ~to_:
                            [ Node.{ id = fresh_id (); instr = `Assign (var, expr) }
                            ; { node with instr = replace_instr node.instr ~var ~expr }
                            ])
                  in
                  (* Transform original node *)
                  let flowgraph =
                    Flowgraph.update
                      flowgraph
                      ~from:node
                      ~to_:{ node with instr = replace_instr node.instr ~var ~expr }
                  in
                  flowgraph)
            | exception _ -> flowgraph)
      in
      { flowgraph; entry })
;;

let copy_prop =
  fix ~f:(fun ir ~continue ->
      print_endline "doing copy_prop";
      let copy_reach = Copy_reach.analysis ir in
      Transform.map ir ~f:(fun node ->
          let copies = (copy_reach node).in_ in
          let unique_copies =
            Set.filter copies ~f:(fun copy ->
                not
                  (Set.exists copies ~f:(fun copy' ->
                       Copy.compare copy copy' <> 0
                       && String.compare copy.lhs copy'.lhs = 0)))
            |> Set.to_list
            |> List.map ~f:(fun Copy.{ lhs; rhs } -> lhs, rhs)
            |> Var.Map.of_alist_exn
          in
          let subst var =
            match Map.find unique_copies var with
            | Some var' ->
              continue ();
              print_endline [%string "Copy propgation: propagating %{var} := %{var'}"];
              var'
            | None -> var
          in
          { node with instr = Instruction.subst_ref_var node.instr ~subst }))
;;

let const_prop =
  fix ~f:(fun ir ~continue ->
      print_endline "doing const_prop";
      let reach = Reach.analysis ir in
      Transform.map ir ~f:(fun node ->
          let reach = (reach node).in_ in
          let unique_reach =
            Set.filter reach ~f:(fun node ->
                not
                  (Set.exists reach ~f:(fun node' ->
                       Node.(node <> node')
                       && not (Set.are_disjoint (def node.instr) (def node'.instr)))))
            |> Set.to_list
          in
          (* Is the reaching definition a constant *)
          let const_reach =
            List.filter_map unique_reach ~f:(fun node ->
                Instruction.is_const_assign node.instr)
            |> Var.Map.of_alist_exn
          in
          let subst var =
            match Map.find const_reach var with
            | Some n ->
              continue ();
              print_endline
                [%string "Constant propgation: propagating %{var} := %{n#Int}"];
              Expr.Int n
            | None -> Expr.Var var
          in
          { node with instr = Instruction.subst_expr node.instr ~subst }))
;;

let peephole_data ir ~continue:_ =
  Transform.filter_map ir ~f:(fun node ->
      match node.instr with
      | `Assign (x, Var y) when Var.(x = y) ->
        print_endline [%string "Elimating instruction %{x} := %{x} (%{node.id#Int})"];
        None
      | `Assign (x, expr) -> Some { node with instr = `Assign (x, Expr.simplify expr) }
      | _ -> Some node)
;;

let peephole_control { flowgraph; entry } ~continue:_ =
  let if_simpl ~cond ~node ~target ~next =
    print_endline [%string "Elimating constant condition (%{node.Node.id#Int})"];
    (* Remove branch edge *)
    let flowgraph =
      if cond
      then Flowgraph.remove_edge flowgraph ~src:node ~dst:next
      else Flowgraph.remove_edge flowgraph ~src:node ~dst:target
    in
    (* Remove current node, preserving connectivity *)
    Flowgraph.remove_and_preserve_connectivity flowgraph node
  in
  let flowgraph =
    Flowgraph.fold flowgraph ~init:flowgraph ~f:(fun node flowgraph ->
        match node.instr with
        | `Cond (cond, label) when Condition.is_const cond ->
          let cond = Condition.eval_const_cond cond in
          (match Flowgraph.succ flowgraph node with
          | [ ({ instr = `Label label'; _ } as target); next ] when Label.(label = label')
            -> if_simpl ~cond ~node ~target ~next
          | [ next; ({ instr = `Label label'; _ } as target) ] when Label.(label = label')
            -> if_simpl ~cond ~node ~target ~next
          (* Conditional branches should always have 2 branches *)
          | _ -> assert false)
        | _ -> flowgraph)
  in
  { flowgraph; entry }
;;

let all ts ir ~continue = List.fold ts ~init:ir ~f:(fun ir t -> t ir ~continue)
let single t ir = t ir ~continue:(fun () -> ())

let aggressive =
  single
    (fix
       ~f:
         (all
            [ dead_code_elim
            ; unreachable_elim
            ; common_subexpr_elim
            ; copy_prop
            ; const_prop
            ; peephole_data
            ; peephole_control
            ]))
;;
