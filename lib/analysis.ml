open Core
module Ir = Ir.Simple
open Ir

let unreachable_nodes { flowgraph; entry } =
  let visited = Hash_set.create (module Ir.Node) in
  (* Traverse the flowgraph [flowgraph] *)
  let rec loop node =
    if not (Hash_set.mem visited node)
    then (
      (* Visit node *)
      Hash_set.add visited node;
      (* Visit children/successors *)
      Flowgraph.succ flowgraph node |> List.iter ~f:loop)
  in
  loop entry;
  Flowgraph.nodes flowgraph
  |> List.filter ~f:(fun node -> not (Hash_set.mem visited node))
;;

module Dataflow = struct
  type ('a, 'cmp) node =
    { in_ : ('a, 'cmp) Set.t
    ; out : ('a, 'cmp) Set.t
    }

  let equal_node node1 node2 =
    Set.equal node1.in_ node2.in_ && Set.equal node1.out node2.out
  ;;

  let sexp_of_node sexp_of_a { in_; out } =
    let in_ = List.sexp_of_t sexp_of_a (Set.to_list in_) in
    let out = List.sexp_of_t sexp_of_a (Set.to_list out) in
    [%sexp (in_ : Sexp.t), (out : Sexp.t)]
  ;;

  type ('a, 'cmp) t = Node.t -> ('a, 'cmp) node

  let create table = Map.find_exn table

  let fix ~init ~f =
    let rec loop map =
      let map' = f map in
      if Map.equal equal_node map map' then map else loop map'
    in
    loop init
  ;;

  let analysis
      { flowgraph; _ }
      ~(init : (_, _) t)
      ~(f : Node.t -> (_, _) t -> (_, _) node)
    =
    let table =
      Flowgraph.fold flowgraph ~init:Node.Map.empty ~f:(fun node table ->
          Map.set table ~key:node ~data:(init node))
    in
    let table =
      fix ~init:table ~f:(fun table ->
          Flowgraph.fold flowgraph ~init:table ~f:(fun node table ->
              let result = create table in
              Map.set table ~key:node ~data:(f node result)))
    in
    create table
  ;;

  let to_dot ir analysis ~sexp_of_a =
    Ir.to_dot ir ~label:(fun node ->
        [%sexp
          (node : Node.t), Analysis, (sexp_of_node sexp_of_a (analysis node) : Sexp.t)]
        |> Sexp.to_string_hum)
  ;;
end

let address_taken_vars flowgraph =
  Instruction.Var.Set.(
    Flowgraph.fold flowgraph ~init:empty ~f:(fun node address_taken_vars ->
        match node.instr with
        | `Address_of (_, `Var x) -> add address_taken_vars x
        | _ -> address_taken_vars))
;;

module Liveness = struct
  open Dataflow
  open Instruction
  module Set = Var.Set

  type live = (Var.t, Var.comparator_witness) Dataflow.t

  let analysis ({ flowgraph; _ } as ir) : live =
    let address_taken_vars = address_taken_vars flowgraph in
    fun node ->
      analysis
        ir
        ~init:(fun _ -> Set.{ in_ = empty; out = empty })
        ~f:(fun node live ->
          let def = def node.instr in
          let ref_ = ref_ ~address_taken_vars node.instr in
          let out_live =
            Flowgraph.succ flowgraph node
            |> List.fold_left ~init:Set.empty ~f:(fun out_live node' ->
                   Set.union out_live (live node').in_)
          in
          let in_live = Set.(union (diff out_live def) ref_) in
          { in_ = in_live; out = out_live })
        node
  ;;
end

let inter_list sets =
  match sets with
  | init :: sets -> List.fold_left sets ~init ~f:Set.inter
  | _ -> assert false
;;

module Avail = struct
  open Dataflow
  open Instruction
  module Set = Expr.Set

  let all_exprs flowgraph =
    Flowgraph.fold flowgraph ~init:Set.empty ~f:(fun node exprs ->
        Set.union_list (List.map (Instruction.exprs node.instr) ~f:Expr.sub_exprs)
        |> Set.union exprs)
  ;;

  type avail = (Expr.t, Expr.comparator_witness) Dataflow.t

  let kill ~all_exprs instr =
    let def = Instruction.def instr in
    Expr.Set.filter all_exprs ~f:(fun expr ->
        not (Var.Set.are_disjoint (Expr.free_vars expr) def))
  ;;

  let gen ~all_exprs instr =
    let exprs = Instruction.exprs instr |> List.map ~f:Expr.sub_exprs |> Set.union_list in
    Expr.Set.diff exprs (kill ~all_exprs instr)
  ;;

  let analysis ({ flowgraph; entry } as ir) =
    let all_exprs = all_exprs flowgraph in
    analysis
      ir
      ~init:(fun node ->
        if Node.compare node entry = 0
        then { in_ = Set.empty; out = Set.empty }
        else { in_ = all_exprs; out = Set.empty })
      ~f:(fun node avail ->
        let out_avail (node : Node.t) =
          Set.diff (avail node).in_ (kill ~all_exprs node.instr)
          |> Set.union (gen ~all_exprs node.instr)
        in
        let in_avail (node : Node.t) =
          let pred = Flowgraph.pred flowgraph node in
          if List.is_empty pred
          then Set.empty
          else pred |> List.map ~f:out_avail |> inter_list
        in
        { in_ = in_avail node; out = out_avail node })
  ;;
end

module Reach = struct
  open Dataflow
  open Instruction
  module Set = Node.Set

  type reach = (Node.t, Node.comparator_witness) Dataflow.t

  let def_sites flowgraph =
    let def_sites = Hashtbl.create (module Var) in
    Flowgraph.iter flowgraph ~f:(fun node ->
        Var.Set.iter (Instruction.def node.instr) ~f:(fun var ->
            Hashtbl.update def_sites var ~f:(function
                | None -> Set.singleton node
                | Some nodes -> Set.add nodes node)));
    let result = def_sites |> Var.Map.of_hashtbl_exn in
    Map.iteri result ~f:(fun ~key ~data ->
        let nodes = Node.Set.to_list data |> List.map ~f:(fun node -> node.Node.id) in
        [%sexp Key, (key : Var.t), Nodes, (nodes : int list)]
        |> Sexp.to_string_hum
        |> print_endline);
    result
  ;;

  let kill (node : Node.t) ~def_sites =
    let defs =
      Instruction.def node.instr
      |> Var.Set.to_list
      |> List.map ~f:(Map.find_exn def_sites)
      |> Node.Set.union_list
    in
    Set.remove defs node
  ;;

  let gen (node : Node.t) =
    if Var.Set.is_empty (Instruction.def node.instr)
    then Node.Set.empty
    else Node.Set.singleton node
  ;;

  let analysis ({ flowgraph; _ } as ir) =
    let def_sites = def_sites flowgraph in
    analysis
      ir
      ~init:(fun _ -> { in_ = Node.Set.empty; out = Node.Set.empty })
      ~f:(fun node reach ->
        let out_reach (node : Node.t) =
          Set.union (gen node) (Set.diff (reach node).in_ (kill ~def_sites node))
        in
        let in_reach (node : Node.t) =
          Flowgraph.pred flowgraph node
          |> List.map ~f:(fun node -> out_reach node)
          |> Node.Set.union_list
        in
        { in_ = in_reach node; out = out_reach node })
  ;;
end

module Copy = struct
  open Instruction

  module T = struct
    type t =
      { lhs : variable
      ; rhs : variable
      }
    [@@deriving compare, hash, sexp]
  end

  include T
  include Comparable.Make (T)
end

module Copy_reach = struct
  open Dataflow
  open Instruction
  module Set = Copy.Set

  type copy_reach = (Copy.t, Copy.comparator_witness) Dataflow.t

  let gen (node : Node.t) =
    Copy.(
      match node.instr with
      | `Assign (x, Var y) -> Set.singleton { lhs = x; rhs = y }
      | _ -> Set.empty)
  ;;

  let kill (node : Node.t) =
    let defs = Instruction.def node.instr |> Var.Set.to_list in
    List.cartesian_product defs defs
    |> List.map ~f:(fun (lhs, rhs) -> Copy.{ lhs; rhs })
    |> Copy.Set.of_list
  ;;

  let all_copies flowgraph =
    Copy.(
      Flowgraph.fold flowgraph ~init:Set.empty ~f:(fun node copies ->
          Set.union (gen node) copies))
  ;;

  let analysis ({ flowgraph; entry } as ir) =
    let all_copies = all_copies flowgraph in
    let inter_list sets =
      match sets with
      | init :: sets -> List.fold_left sets ~init ~f:Set.inter
      | _ -> assert false
    in
    analysis
      ir
      ~init:(fun node ->
        if Node.compare node entry = 0
        then { in_ = Copy.Set.empty; out = gen entry }
        else { in_ = all_copies; out = all_copies })
      ~f:(fun node copy_reach ->
        let out_copy_reach (node : Node.t) =
          Set.diff (copy_reach node).in_ (kill node) |> Set.union (gen node)
        in
        let in_copy_reach (node : Node.t) =
          let pred = Flowgraph.pred flowgraph node in
          if List.is_empty pred
          then Copy.Set.empty
          else pred |> List.map ~f:out_copy_reach |> inter_list
        in
        { in_ = in_copy_reach node; out = out_copy_reach node })
  ;;
end