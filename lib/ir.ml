open Core
open Flowgraph
open Instruction

let fresh_id =
  let next = ref 0 in
  fun () ->
    next := !next + 1;
    !next
;;

module type S = sig
  module Node : Node
  module Flowgraph : Flowgraph.S with type node := Node.t

  type t =
    { entry : Node.t
    ; flowgraph : Flowgraph.t
    }

  val to_dot : t -> label:(Node.t -> string) -> string

  module Transform : sig
    val map : t -> f:(Node.t -> Node.t) -> t

    (** Warning: cannot map entry to None! *)
    val filter_map : t -> f:(Node.t -> Node.t option) -> t

    (** Warning: cannot map to empty list! *)
    val map_many : t -> f:(Node.t -> Node.t list) -> t

    val mapf : t -> f:(Flowgraph.t -> Node.t -> Node.t) -> t
  end
end

module Make (Node : Node) : S with module Node := Node = struct
  module Node = Node
  module Flowgraph = Flowgraph.Make (Node)

  type t =
    { entry : Node.t
    ; flowgraph : Flowgraph.t
    }

  let to_dot t ~label = Flowgraph.to_dot t.flowgraph ~label

  module Transform = struct
    let map { flowgraph; entry } ~f =
      let new_entry = f entry in
      let f node = if Node.compare node entry = 0 then new_entry else f node in
      { flowgraph = Flowgraph.map flowgraph ~f; entry = new_entry }
    ;;

    let filter_map { flowgraph; entry } ~f =
      let new_entry =
        Option.value_exn
          (f entry)
          ~here:[%here]
          ~error:(Error.create_s [%message "Cannot map entry to None" (entry : Node.t)])
      in
      let f node = if Node.compare node entry = 0 then Some new_entry else f node in
      { flowgraph = Flowgraph.filter_map flowgraph ~f; entry }
    ;;

    let map_many { flowgraph; entry } ~f =
      let new_entry = f entry in
      let f node = if Node.compare node entry = 0 then new_entry else f node in
      { flowgraph = Flowgraph.map_many flowgraph ~f; entry = List.hd_exn new_entry }
    ;;

    let mapf { flowgraph; entry } ~f =
      let new_entry = f flowgraph entry in
      let f flowgraph node =
        if Node.compare node entry = 0 then new_entry else f flowgraph node
      in
      { flowgraph = Flowgraph.mapf flowgraph ~f; entry = new_entry }
    ;;
  end
end

module Simple = struct
  module Node = struct
    module T = struct
      type t =
        { id : int
        ; instr : instruction
        }
      [@@deriving sexp]

      let compare t1 t2 = Int.compare t1.id t2.id
      let hash t = Int.hash t.id
      let hash_fold_t state t = Int.hash_fold_t state t.id
    end

    include T
    include Comparable.Make (T)
  end

  include Make (Node)

  let of_program program =
    let nodes =
      List.mapi program ~f:(fun addr instr -> addr, Node.{ id = fresh_id (); instr })
      |> Int.Map.of_alist_exn
    in
    let find_node addr = Map.find_exn nodes addr in
    (* Entry is the first instruction. *)
    let entry = find_node 0 in
    (* Compute a mapping between labels and addresses. *)
    let labels =
      Map.fold nodes ~init:String.Map.empty ~f:(fun ~key:addr ~data:node labels ->
          match node.instr with
          | `Label label -> String.Map.set labels ~key:label ~data:addr
          | _ -> labels)
    in
    (* Helper functions *)
    let find_label label = find_node (Map.find_exn labels label) in
    (* Compute a set of addresses of address taken labels (used for indirect jumps) *)
    let address_taken_labels =
      Map.fold nodes ~init:Int.Set.empty ~f:(fun ~key:_ ~data:node addressed_labels ->
          match node.instr with
          | `Address_of (_, `Label label) ->
            (try Int.Set.add addressed_labels (Map.find_exn labels label) with
            | _ -> raise_s [%message "Unbound address" (label : label)])
          | _ -> addressed_labels)
      |> Int.Set.to_list
    in
    (* Add blocks to [flowgraph] *)
    let flowgraph =
      List.fold_left (Map.data nodes) ~init:Flowgraph.empty ~f:Flowgraph.add_node
    in
    (* Add basic edges to [flowgraph] *)
    let flowgraph =
      Map.fold nodes ~init:flowgraph ~f:(fun ~key:addr ~data:node flowgraph ->
          match node.instr with
          (* Jumps and returns have no basic control flow.  *)
          | `Jump _ | `Jump_indir _ | `Ret -> flowgraph
          | _ ->
            let next_node = find_node (addr + 1) in
            Flowgraph.add_edge flowgraph ~src:node ~dst:next_node)
    in
    (* Add non-trivial edges to [flowgraph] *)
    let flowgraph =
      Map.fold nodes ~init:flowgraph ~f:(fun ~key:_ ~data:node flowgraph ->
          match node.instr with
          (* Statically known control flow *)
          | `Cond (_, label) | `Call (label, _) | `Jump label ->
            let target_node = find_label label in
            Flowgraph.add_edge flowgraph ~src:node ~dst:target_node
          (* Indirect jumps, not statically known. Approximate using [addressed_labels] *)
          | `Calli _ | `Jump_indir _ ->
            address_taken_labels
            |> List.map ~f:find_node
            |> List.fold_left ~init:flowgraph ~f:(fun flowgraph target_node ->
                   Flowgraph.add_edge flowgraph ~src:node ~dst:target_node)
          (* All other instructions have trivial/basic control flow *)
          | _ -> flowgraph)
    in
    { entry; flowgraph }
  ;;
end

module Basic_block = struct
  module Node = struct
    module T = struct
      type t =
        { id : int
        ; block : instruction list
        }
      [@@deriving sexp]

      let compare t1 t2 = Int.compare t1.id t2.id
      let hash t = Int.hash t.id
      let hash_fold_t state t = Int.hash_fold_t state t.id
    end

    include T
    include Comparable.Make (T)

    let of_leader leader =
      { id = fresh_id ()
      ; block = List.map leader ~f:(fun node -> node.Simple.Node.instr)
      }
    ;;
  end

  include Make (Node)
  module Leader_list = Graph.Leaderlist.Make (Simple.Flowgraph.To_ocamlgraph)

  let of_simple Simple.({ flowgraph; entry } as simple) =
    let leaders = Leader_list.leader_lists flowgraph entry in
    let start_id leader = (List.hd_exn leader).Simple.Node.id in
    let next_ids leader =
      Simple.Flowgraph.succ simple.flowgraph (List.last_exn leader)
      |> List.map ~f:(fun node -> node.id)
    in
    (* Start id to block/nodes mapping *)
    let nodes =
      List.map leaders ~f:(fun leader ->
          start_id leader, (Node.of_leader leader, next_ids leader))
      |> Int.Map.of_alist_exn
    in
    let find_node id = Map.find_exn nodes id |> fst in
    (* Find entry node *)
    let entry = find_node entry.id in
    (* Compute basic block flowgraph from [flowgraph] *)
    (* 1. Add nodes/blocks to graph *)
    let flowgraph =
      List.fold_left (Map.data nodes) ~init:Flowgraph.empty ~f:(fun flowgraph (node, _) ->
          Flowgraph.add_node flowgraph node)
    in
    (* 2. Add edges to graph *)
    let flowgraph =
      List.fold_left
        (Map.data nodes)
        ~init:flowgraph
        ~f:(fun flowgraph (node, next_ids) ->
          List.fold_left next_ids ~init:flowgraph ~f:(fun flowgraph next_id ->
              Flowgraph.add_edge flowgraph ~src:node ~dst:(find_node next_id)))
    in
    { entry; flowgraph }
  ;;

  let of_program program = Simple.of_program program |> of_simple
end

module Ssa = struct
  module Node = struct
    module T = struct
      type t =
        { id : int
        ; mutable phis : [ `Phi of variable * variable list ] list
        ; mutable block : instruction list
        }
      [@@deriving sexp]

      let compare t1 t2 = Int.compare t1.id t2.id
      let hash t = Int.hash t.id
      let hash_fold_t state t = Int.hash_fold_t state t.id
    end

    include T
    include Comparable.Make (T)
  end

  include Make (Node)

  module Dominator = Graph.Dominator.Make_graph (struct
    include Flowgraph.To_ocamlgraph

    let empty () = empty
  end)

  let create_init_ir Basic_block.{ flowgraph; entry } =
    let nodes = Hashtbl.create (module Int) in
    let find_node (node : Basic_block.Node.t) = Hashtbl.find_exn nodes node.id in
    let ssa_flowgraph =
      Basic_block.Flowgraph.fold flowgraph ~init:Flowgraph.empty ~f:(fun node flowgraph ->
          let node' = Node.{ id = node.id; phis = []; block = node.block } in
          Hashtbl.set nodes ~key:node.id ~data:node';
          Flowgraph.add_node flowgraph node')
    in
    let ssa_flowgraph =
      Basic_block.Flowgraph.fold_edges
        flowgraph
        ~init:ssa_flowgraph
        ~f:(fun ~src ~dst flowgraph ->
          let src = find_node src
          and dst = find_node dst in
          Flowgraph.add_edge flowgraph ~src ~dst)
    in
    { flowgraph = ssa_flowgraph; entry = find_node entry }
  ;;

  let place_phi_nodes { flowgraph; _ } ~dom_frontier =
    (* Variables defined by a node (memoized) *)
    let def_node =
      let table = Hashtbl.create (module Node) in
      fun node ->
        Hashtbl.find_or_add table node ~default:(fun () ->
            node.block |> List.map ~f:def |> Var.Set.union_list)
    in
    (* Initialization of def_sites *)
    let def_sites = Hashtbl.create (module Var) in
    Flowgraph.iter flowgraph ~f:(fun node ->
        Set.iter (def_node node) ~f:(fun var ->
            Hashtbl.update def_sites var ~f:(function
                | None -> Set.singleton (module Node) node
                | Some nodes -> Set.add nodes node)));
    (* Main loop for loop *)
    let phis = Hashtbl.create (module Var) in
    let find_phis a =
      Hashtbl.find_or_add phis a ~default:(fun () -> Set.empty (module Node))
    in
    List.iter (Hashtbl.to_alist def_sites) ~f:(fun (a, def_sites) ->
        (* make phi node for [a] *)
        let make_phi node =
          let preds = List.length (Flowgraph.pred flowgraph node) in
          let vars = List.init preds ~f:(Fn.const a) in
          `Phi (a, vars)
        in
        (* Main while loop *)
        let rec loop worklist =
          match worklist with
          | [] -> ()
          | node :: worklist ->
            let worklist = ref worklist in
            let schedule node = worklist := node :: !worklist in
            List.iter (dom_frontier node) ~f:(fun node' ->
                let a_phis = find_phis a in
                if not (Set.mem a_phis node')
                then (
                  (* Insert phi node *)
                  node'.phis <- make_phi node' :: node'.phis;
                  (* Update phis *)
                  Hashtbl.set phis ~key:a ~data:(Set.add a_phis node');
                  (* Schedule additional nodes *)
                  if not (Set.mem (def_node node') a) then schedule node'));
            loop !worklist
        in
        loop (Set.to_list def_sites))
  ;;

  let index_of t x ~compare =
    List.findi t ~f:(fun _ y -> compare x y = 0) |> Option.value_exn |> fst
  ;;

  let rename { flowgraph; entry } ~dom_tree =
    (* Compute [defs] of a node *)
    let defs_of_node =
      Flowgraph.fold
        flowgraph
        ~init:(Map.empty (module Node))
        ~f:(fun node defs_of_node ->
          Map.set
            defs_of_node
            ~key:node
            ~data:
              ((* Implementation warning: assumes a variable cannot be defined twice in an instruction *)
               let defs_in_block =
                 node.block
                 |> List.map ~f:(fun instr -> def instr |> Set.to_list)
                 |> List.join
               and defs_in_phis = node.phis |> List.map ~f:(fun (`Phi (x, _)) -> x) in
               defs_in_block @ defs_in_phis))
    in
    (* Compute all variables (used for initialization) *)
    let all_vars =
      Flowgraph.fold flowgraph ~init:Var.Set.empty ~f:(fun node all_vars ->
          let vars =
            List.map node.block ~f:Instruction.free_vars_all |> Var.Set.union_list
          in
          Var.Set.union vars all_vars)
    in
    (* Initialization *)
    let counts = Hashtbl.create (module Var)
    and stacks = Hashtbl.create (module Var) in
    Set.iter all_vars ~f:(fun var ->
        Hashtbl.set counts ~key:var ~data:0;
        Hashtbl.set stacks ~key:var ~data:(Stack.singleton 0));
    (* [version var i] returns [var ^ "_" ^ i]. *)
    let version var i = var ^ "_" ^ Int.to_string i in
    (* [subst_var_def var] performs the following:
        - increments counter for [var], 
        - pushes new version onto stack, and
        - return var 
    *)
    let subst_var_def var =
      let i =
        (* Increment counter/version for [var] *)
        Hashtbl.set counts ~key:var ~data:(Hashtbl.find_exn counts var + 1);
        Hashtbl.find_exn counts var
      in
      (* Update stack *)
      Stack.push (Hashtbl.find_exn stacks var) i;
      (* Return version *)
      version var i
    in
    (* [subst_var_ref var] returns current version of [var] *)
    let subst_var_ref var =
      (* Get current version *)
      let i = Stack.top_exn (Hashtbl.find_exn stacks var) in
      version var i
    in
    (* Rename *)
    let rec rename (node : Node.t) =
      (* Update phis (defs only) *)
      node.phis
        <- List.map node.phis ~f:(fun (`Phi (var, exprs)) ->
               `Phi (subst_var_def var, exprs));
      (* Update block *)
      node.block
        <- List.map node.block ~f:(fun instr ->
               let instr = Instruction.subst_ref_var instr ~subst:subst_var_ref in
               Instruction.subst_def_var instr ~subst:subst_var_def);
      (* Update phi (refs only) *)
      List.iter (Flowgraph.succ flowgraph node) ~f:(fun succ ->
          let preds = Flowgraph.pred flowgraph succ in
          let j = index_of preds node ~compare:Node.compare in
          let update_phi (`Phi (x, vars)) =
            `Phi
              ( x
              , List.mapi vars ~f:(fun i var -> if i = j then subst_var_ref var else var)
              )
          in
          succ.phis <- List.map succ.phis ~f:update_phi);
      (* Traverse children on dom_tree *)
      List.iter (dom_tree node) ~f:rename;
      (* Pop stack for original defs in [node] *)
      List.iter (Map.find_exn defs_of_node node) ~f:(fun var ->
          ignore (Stack.pop_exn (Hashtbl.find_exn stacks var) : int))
    in
    rename entry
  ;;

  let of_basic_block ir =
    let ({ flowgraph; entry } as ir) = create_init_ir ir in
    let all = Dominator.compute_all flowgraph entry in
    place_phi_nodes ir ~dom_frontier:all.dom_frontier;
    rename ir ~dom_tree:all.dom_tree;
    ir
  ;;

  let of_simple simple = simple |> Basic_block.of_simple |> of_basic_block
  let of_program program = program |> Simple.of_program |> of_simple
end
