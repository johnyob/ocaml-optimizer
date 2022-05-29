open Core
open Instruction
open Ir
module Ir = Ir.Simple
open Ir

module Node = struct
  include Node
  include Comparable.Make (Node)
end

type ('a, 'cmp) dataflow =
  { in_ : ('a, 'cmp) Set.t
  ; out : ('a, 'cmp) Set.t
  }

let dataflow_equal t1 t2 = Set.equal t1.in_ t2.in_ && Set.equal t1.out t2.out

type ('a, 'cmp) result = Node.t -> ('a, 'cmp) dataflow

let create_result table = Map.find_exn table

let fix ~init ~f =
  let rec loop map =
    let map' = f map in
    if Map.equal dataflow_equal map map' then map else loop map'
  in
  loop init
;;

let analysis
    { flowgraph; _ }
    ~(init : (_, _) result)
    ~(f : Node.t -> (_, _) result -> (_, _) dataflow)
  =
  let table =
    Flowgraph.fold flowgraph ~init:Node.Map.empty ~f:(fun node table ->
        Map.set table ~key:node ~data:(init node))
  in
  let table =
    fix ~init:table ~f:(fun table ->
        Flowgraph.fold flowgraph ~init:table ~f:(fun node table ->
            let result = create_result table in
            Map.set table ~key:node ~data:(f node result)))
  in
  create_result table
;;

let to_dot ir analysis ~sexp_of_a =
  let sexp_of_dataflow dataflow =
    let in_ = List.sexp_of_t sexp_of_a (Set.to_list dataflow.in_) in
    let out = List.sexp_of_t sexp_of_a (Set.to_list dataflow.out) in
    [%sexp (in_ : Sexp.t), (out : Sexp.t)]
  in
  Ir.to_dot ir ~label:(fun node ->
      [%sexp (node : Node.t), Analysis, (sexp_of_dataflow (analysis node) : Sexp.t)]
      |> Sexp.to_string_hum)
;;

let address_taken_vars flowgraph =
  Flowgraph.fold flowgraph ~init:String.Set.empty ~f:(fun node address_taken_vars ->
      match node.instr with
      | `Address_of (_, `Var x) -> Set.add address_taken_vars x
      | _ -> address_taken_vars)
;;

module Liveness = struct
  module Set = String.Set

  type live = Node.t -> (string, String.comparator_witness) dataflow

  let analysis ({ flowgraph; _ } as ir) =
    let address_taken_vars = address_taken_vars flowgraph in
    analysis
      ir
      ~init:(fun _ -> String.Set.{ in_ = empty; out = empty })
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
  ;;
end

module Avail = struct
  open Instruction

  module Expr = struct
    module T = struct
      type t = expression =
        | Var of variable
        | Int of int
        | Plus of t * t
      [@@deriving compare, hash, sexp]
    end

    include T
    include Comparable.Make (T)

    let rec size expr =
      match expr with
      | Var _ | Int _ -> 1
      | Plus (expr1, expr2) -> 1 + size expr1 + size expr2
    ;;

    let rec sub_exprs expr =
      match expr with
      | Var _ | Int _ -> Set.empty
      | Plus ((Var _ | Int _), (Var _ | Int _)) -> Set.singleton expr
      | Plus (expr1, expr2) ->
        Set.add (Set.union (sub_exprs expr1) (sub_exprs expr2)) expr
    ;;

    let exprs_of_cond cond =
      let open Instruction in
      match cond with
      | Equal (expr1, expr2)
      | Greater_than (expr1, expr2)
      | Less_than (expr1, expr2)
      | Not_equal (expr1, expr2) -> [ expr1; expr2 ]
    ;;

    let exprs_of_instr instr =
      match instr with
      | `Assign (_, expr) | `Deref (_, expr) | `Store (expr, _) -> [ expr ]
      | `Call (_, exprs) | `Calli (_, exprs) -> exprs
      | `Cond (cond, _) -> exprs_of_cond cond
      | _ -> []
    ;;

    let all_exprs flowgraph =
      Flowgraph.fold flowgraph ~init:Set.empty ~f:(fun node exprs ->
          Set.union_list (List.map (exprs_of_instr node.instr) ~f:sub_exprs)
          |> Set.union exprs)
    ;;
  end

  type avail = Node.t -> (Expr.t, Expr.comparator_witness) dataflow

  let kill ~all_exprs instr =
    let def = def instr in
    Expr.Set.filter all_exprs ~f:(fun expr ->
        not (String.Set.are_disjoint (free_vars_expr expr) def))
  ;;

  let gen ~all_exprs instr =
    let exprs = Expr.(exprs_of_instr instr |> List.map ~f:sub_exprs |> Set.union_list) in
    Expr.Set.diff exprs (kill ~all_exprs instr)
  ;;

  let analysis ({ flowgraph; entry } as ir) =
    let module Set = Expr.Set in
    let all_exprs = Expr.all_exprs flowgraph in
    print_endline "All_exprs:";
    [%sexp_of: Expr.Set.t] all_exprs |> Sexp.to_string_hum |> print_endline;
    let inter_list sets =
      match sets with
      | init :: sets -> List.fold_left sets ~init ~f:Set.inter
      | _ -> assert false
    in
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
  type avail = Node.t -> (Node.t, Node.comparator_witness) dataflow

  let def_sites flowgraph =
    let def_sites = Hashtbl.create (module String) in
    Flowgraph.iter flowgraph ~f:(fun node ->
        Set.iter (def node.instr) ~f:(fun var ->
            Hashtbl.update def_sites var ~f:(function
                | None -> Set.singleton (module Node) node
                | Some nodes -> Set.add nodes node)));
    def_sites |> Map.of_hashtbl_exn (module String)
  ;;

  let kill (node : Node.t) ~def_sites =
    let defs =
      def node.instr
      |> Set.to_list
      |> List.map ~f:(Map.find_exn def_sites)
      |> Set.union_list (module Node)
    in
    Set.remove defs node
  ;;

  let gen node = Set.singleton (module Node) node

  let analysis ({ flowgraph; _ } as ir) =
    let def_sites = def_sites flowgraph in
    analysis
      ir
      ~init:(fun _ -> { in_ = Set.empty (module Node); out = Set.empty (module Node) })
      ~f:(fun node reach ->
        let out_reach (node : Node.t) =
          Set.diff (reach node).in_ (kill ~def_sites node) |> Set.union (gen node)
        in
        let in_reach (node : Node.t) =
          Flowgraph.pred flowgraph node
          |> List.map ~f:(fun node -> (reach node).out)
          |> Set.union_list (module Node)
        in
        { in_ = in_reach node; out = out_reach node })
  ;;
end

module Copy_reach = struct
  module Copy = struct
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

  type copy_reach = Node.t -> (Copy.t, Copy.comparator_witness) dataflow

  let gen (node : Node.t) =
    Copy.(
      match node.instr with
      | `Assign (x, Var y) -> Set.singleton { lhs = x; rhs = y }
      | _ -> Set.empty)
  ;;

  let kill (node : Node.t) =
    let defs = def node.instr |> Set.to_list in
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

module Transform = struct
  let fix flowgraph ~f =
    (* true when no transforms have been completed by [f] *)
    let is_done = ref false in
    let continue () = is_done := false in
    let flowgraph = ref flowgraph in
    while not !is_done do
      is_done := true;
      flowgraph := f !flowgraph ~continue
    done;
    !flowgraph
  ;;

  type t = Ir.t -> Ir.t

  let remove_node flowgraph node =
    let pred = Flowgraph.pred flowgraph node
    and succ = Flowgraph.succ flowgraph node in
    let flowgraph = Flowgraph.remove_node flowgraph node in
    List.cartesian_product pred succ
    |> List.fold_left ~init:flowgraph ~f:(fun flowgraph (src, dst) ->
           Flowgraph.add_edge flowgraph ~src ~dst)
  ;;

  let dead_code_elim =
    fix ~f:(fun ({ flowgraph; entry } as ir) ~continue ->
        let live = Liveness.analysis ir in
        let is_dead (node : Node.t) =
          let def = def node.instr in
          let out_live = (live node).out in
          Set.for_all def ~f:(fun x -> not (Set.mem out_live x))
        in
        let flowgraph =
          Flowgraph.fold flowgraph ~init:flowgraph ~f:(fun node flowgraph ->
              if is_dead node
              then (
                continue ();
                remove_node flowgraph node)
              else flowgraph)
        in
        { flowgraph; entry })
  ;;

  (* CSE requires:
      - avail
      - elim_expr ~expr' ~var expr
      - reaching_exprs
      -  *)

  let reaching_exprs flowgraph node ~expr =
    let visited = Hash_set.create (module Node) in
    (* [node] is initially visited *)
    Hash_set.add visited node;
    let reaching_exprs = Hash_set.create (module Node) in
    let rec loop node =
      if not (Hash_set.mem visited node)
      then (
        print_endline ("visiting " ^ Int.to_string node.id);
        (* visit *)
        Hash_set.add visited node;
        (* [expr] not invalidated *)
        if Set.are_disjoint (free_vars_expr expr) (def node.instr)
        then
          (* [node] computes [expr] *)
          if Avail.Expr.(
               Set.mem
                 (Set.union_list (List.map (exprs_of_instr node.instr) ~f:sub_exprs))
                 expr)
          then Hash_set.add reaching_exprs node
          else Flowgraph.pred flowgraph node |> List.iter ~f:loop)
    in
    (* traverse from predecessors *)
    List.iter (Flowgraph.pred flowgraph node) ~f:loop;
    Hash_set.to_list reaching_exprs
  ;;

  let rec elim_expr_expr expr ~var ~expr:expr' =
    if Avail.Expr.compare expr expr' = 0
    then Var var
    else (
      match expr with
      | Plus (expr1, expr2) ->
        Plus (elim_expr_expr expr1 ~var ~expr:expr', elim_expr_expr expr2 ~var ~expr:expr')
      | Var _ | Int _ -> expr)
  ;;

  let elim_expr_cond cond ~var ~expr =
    match cond with
    | Equal (expr1, expr2) ->
      Equal (elim_expr_expr expr1 ~var ~expr, elim_expr_expr expr2 ~var ~expr)
    | Not_equal (expr1, expr2) ->
      Not_equal (elim_expr_expr expr1 ~var ~expr, elim_expr_expr expr2 ~var ~expr)
    | Less_than (expr1, expr2) ->
      Less_than (elim_expr_expr expr1 ~var ~expr, elim_expr_expr expr2 ~var ~expr)
    | Greater_than (expr1, expr2) ->
      Greater_than (elim_expr_expr expr1 ~var ~expr, elim_expr_expr expr2 ~var ~expr)
  ;;

  let elim_expr_instr instr ~var ~expr =
    match instr with
    | `Assign (x, expr') -> `Assign (x, elim_expr_expr expr' ~var ~expr)
    | `Deref (x, expr') -> `Deref (x, elim_expr_expr expr' ~var ~expr)
    | `Store (expr', x) -> `Store (elim_expr_expr expr' ~var ~expr, x)
    | `Call (label, exprs) -> `Call (label, List.map exprs ~f:(elim_expr_expr ~var ~expr))
    | `Calli (x, exprs) -> `Calli (x, List.map exprs ~f:(elim_expr_expr ~var ~expr))
    | `Cond (cond, label) -> `Cond (elim_expr_cond cond ~var ~expr, label)
    | instr -> instr
  ;;

  let cse_update flowgraph node ~var ~expr =
    let pred = Flowgraph.pred flowgraph node
    and succ = Flowgraph.succ flowgraph node in
    let flowgraph = Flowgraph.remove_node flowgraph node in
    (* Add new node *)
    let node' = { node with instr = elim_expr_instr node.instr ~var ~expr } in
    let flowgraph = Flowgraph.add_node flowgraph node' in
    (* Add edges  *)
    let flowgraph =
      List.fold_left pred ~init:flowgraph ~f:(fun flowgraph src ->
          Flowgraph.add_edge flowgraph ~src ~dst:node')
    in
    let flowgraph =
      List.fold_left succ ~init:flowgraph ~f:(fun flowgraph dst ->
          Flowgraph.add_edge flowgraph ~src:node' ~dst)
    in
    flowgraph
  ;;

  let cse_transform flowgraph node ~var ~expr =
    let pred = Flowgraph.pred flowgraph node
    and succ = Flowgraph.succ flowgraph node in
    let flowgraph = Flowgraph.remove_node flowgraph node in
    (* Create [var := expr] node *)
    let temp = Node.{ id = fresh_id (); instr = `Assign (var, expr) } in
    (* Create new node *)
    let node' = { node with instr = elim_expr_instr node.instr ~var ~expr } in
    (* Add nodes *)
    let flowgraph = Flowgraph.add_node flowgraph temp in
    let flowgraph = Flowgraph.add_node flowgraph node' in
    (* Add edges  *)
    let flowgraph =
      List.fold_left pred ~init:flowgraph ~f:(fun flowgraph src ->
          Flowgraph.add_edge flowgraph ~src ~dst:temp)
    in
    let flowgraph = Flowgraph.add_edge flowgraph ~src:temp ~dst:node' in
    let flowgraph =
      List.fold_left succ ~init:flowgraph ~f:(fun flowgraph dst ->
          Flowgraph.add_edge flowgraph ~src:node' ~dst)
    in
    flowgraph
  ;;

  let common_subexpr_elim =
    fix ~f:(fun ({ flowgraph; entry } as ir) ~continue ->
        let avail = Avail.analysis ir in
        let flowgraph =
          Flowgraph.fold flowgraph ~init:flowgraph ~f:(fun (node : Node.t) flowgraph ->
              match avail node with
              | avail_node ->
                (* print_endline ("found avail for node: " ^ Int.to_string node.id); *)
                let sub_exprs = Avail.Expr.exprs_of_instr node.instr in
                List.filter sub_exprs ~f:(Set.mem avail_node.in_)
                (* Eliminate largest expressions first *)
                |> List.sort
                     ~compare:
                       Avail.(
                         fun expr1 expr2 ->
                           -Int.compare (Expr.size expr1) (Expr.size expr2))
                |> List.fold_left ~init:flowgraph ~f:(fun flowgraph expr ->
                       print_string ("Node: " ^ Int.to_string node.id ^ " removing ");
                       [%sexp_of: Avail.Expr.t] expr
                       |> Sexp.to_string_hum
                       |> print_endline;
                       continue ();
                       (* Find reaching exprs of [expr] *)
                       let reaching_exprs = reaching_exprs flowgraph node ~expr in
                       print_string "reaching exprs: ";
                       [%sexp_of: int list]
                         (List.map reaching_exprs ~f:(fun node -> node.id))
                       |> Sexp.to_string_hum
                       |> print_endline;
                       (* Generate new variable *)
                       let var = "t#" ^ Int.to_string (fresh_id ()) in
                       (* Transform reaching exprs *)
                       let flowgraph =
                         List.fold_left
                           reaching_exprs
                           ~init:flowgraph
                           ~f:(cse_transform ~var ~expr)
                       in
                       (* Transform original node *)
                       let flowgraph = cse_update flowgraph node ~var ~expr in
                       flowgraph)
              | exception _ ->
                (* node must be new since last analysis (ignore) *)
                flowgraph)
        in
        { flowgraph; entry })
  ;;

  let copy_prop_update flowgraph (node : Node.t) ~copy:Copy_reach.Copy.{ lhs; rhs } ~continue =
    (* Hack: [address_taken_vars = []] for free_vars of expressions in instructions *)
    if Set.mem (ref_ ~address_taken_vars:String.Set.empty node.instr) lhs
    then (
      continue ();
      let node' =
        { node with instr = elim_expr_instr node.instr ~var:rhs ~expr:(Var lhs) }
      in
      (* Replace [node] w/ [node'], return new flowgraph *)
      let pred = Flowgraph.pred flowgraph node
      and succ = Flowgraph.succ flowgraph node in
      let flowgraph = Flowgraph.remove_node flowgraph node in
      let flowgraph = Flowgraph.add_node flowgraph node' in
      (* Add edges  *)
      let flowgraph =
        List.fold_left pred ~init:flowgraph ~f:(fun flowgraph src ->
            Flowgraph.add_edge flowgraph ~src ~dst:node')
      in
      let flowgraph =
        List.fold_left succ ~init:flowgraph ~f:(fun flowgraph dst ->
            Flowgraph.add_edge flowgraph ~src:node' ~dst)
      in
      flowgraph, node')
    else flowgraph, node
  ;;

  let copy_prop =
    let module Copy = Copy_reach.Copy in
    fix ~f:(fun ({ flowgraph; entry } as ir) ~continue ->
        let copy_reach = Copy_reach.analysis ir in
        (* Assumes entry is not updated (usually not the case since entry should be a label node) *)
        let flowgraph =
          Flowgraph.fold flowgraph ~init:flowgraph ~f:(fun node flowgraph ->
              let copies = (copy_reach node).in_ in
              (* No other copy of [x] reaches the node for copy [x := y] in [copies] *)
              let unique_copies =
                Set.filter copies ~f:(fun copy ->
                    not
                      (Set.exists copies ~f:(fun copy' ->
                           Copy.compare copy copy' <> 0
                           && String.compare copy.lhs copy'.lhs = 0)))
              in
              Set.fold
                unique_copies
                ~init:(flowgraph, node)
                ~f:(fun (flowgraph, node) copy ->
                  (* continue (); *)
                  copy_prop_update flowgraph node ~copy ~continue)
              |> fst)
        in
        { entry; flowgraph })
  ;;

  let rec is_const_expr expr =
    match expr with
    | Var _ -> false
    | Int _ -> true
    | Plus (expr1, expr2) -> is_const_expr expr1 && is_const_expr expr2
  ;;

  let rec eval_const_expr expr =
    match expr with
    | Var _ -> assert false
    | Int n -> n
    | Plus (expr1, expr2) -> eval_const_expr expr1 + eval_const_expr expr2
  ;;

  let is_const_assign (node : Node.t) =
    match node.instr with
    | `Assign (x, expr) when is_const_expr expr -> Some (x, eval_const_expr expr)
    | _ -> None
  ;;

  let rec subst_expr_expr expr' ~var ~expr =
    match expr' with
    | Var var' when String.(var = var') -> expr
    | Plus (expr1, expr2) ->
      Plus (subst_expr_expr expr1 ~var ~expr, subst_expr_expr expr2 ~var ~expr)
    | expr -> expr
  ;;

  let subst_expr_cond (cond : condition) ~var ~expr : condition =
    match cond with
    | Equal (expr1, expr2) ->
      Equal (subst_expr_expr ~var ~expr expr1, subst_expr_expr ~var ~expr expr2)
    | Greater_than (expr1, expr2) ->
      Greater_than (subst_expr_expr ~var ~expr expr1, subst_expr_expr ~var ~expr expr2)
    | Less_than (expr1, expr2) ->
      Less_than (subst_expr_expr ~var ~expr expr1, subst_expr_expr ~var ~expr expr2)
    | Not_equal (expr1, expr2) ->
      Not_equal (subst_expr_expr ~var ~expr expr1, subst_expr_expr ~var ~expr expr2)
  ;;

  let subst_expr_instr instr ~var ~expr =
    match instr with
    | `Call (lab, exprs) -> `Call (lab, List.map exprs ~f:(subst_expr_expr ~var ~expr))
    | `Calli (x, exprs) -> `Calli (x, List.map exprs ~f:(subst_expr_expr ~var ~expr))
    | `Jump label -> `Jump label
    | `Jump_indir x -> `Jump_indir x
    | `Cond (cond, label) -> `Cond (subst_expr_cond ~var ~expr cond, label)
    | `Ret -> `Ret
    | `Assign (x, expr') -> `Assign (x, subst_expr_expr ~var ~expr expr')
    | `Address_of (x, rhs) -> `Address_of (x, rhs)
    | `Deref (x, expr') -> `Deref (x, subst_expr_expr ~var ~expr expr')
    | `Store (expr', x) -> `Store (subst_expr_expr ~var ~expr expr', x)
    | `Label label -> `Label label
  ;;

  let const_prop_update flowgraph (node : Node.t) ~var ~const ~continue =
    (* Hack: [address_taken_vars = []] for free_vars of expressions in instructions *)
    if Set.mem (ref_ ~address_taken_vars:String.Set.empty node.instr) var
    then (
      continue ();
      let node' =
        { node with instr = subst_expr_instr node.instr ~var ~expr:(Int const) }
      in
      (* Replace [node] w/ [node'], return new flowgraph *)
      let pred = Flowgraph.pred flowgraph node
      and succ = Flowgraph.succ flowgraph node in
      let flowgraph = Flowgraph.remove_node flowgraph node in
      let flowgraph = Flowgraph.add_node flowgraph node' in
      (* Add edges  *)
      let flowgraph =
        List.fold_left pred ~init:flowgraph ~f:(fun flowgraph src ->
            Flowgraph.add_edge flowgraph ~src ~dst:node')
      in
      let flowgraph =
        List.fold_left succ ~init:flowgraph ~f:(fun flowgraph dst ->
            Flowgraph.add_edge flowgraph ~src:node' ~dst)
      in
      flowgraph, node')
    else flowgraph, node
  ;;

  let const_prop =
    fix ~f:(fun ({ flowgraph; entry } as ir) ~continue ->
        let reach = Reach.analysis ir in
        let flowgraph =
          Flowgraph.fold flowgraph ~init:flowgraph ~f:(fun node flowgraph ->
              let reach = (reach node).in_ |> Set.to_list in
              (* Is the reaching definition a constant *)
              let const_reach = List.filter_map reach ~f:is_const_assign in
              List.fold_left
                const_reach
                ~init:(flowgraph, node)
                ~f:(fun (flowgraph, node) (var, const) ->
                  const_prop_update flowgraph node ~var ~const ~continue)
              |> fst)
        in
        { flowgraph; entry })
  ;;
end
