open Core
include Flowgraph_intf

module Make (Node : Node) = struct
  module V = struct
    type t = Node.t [@@deriving compare, hash]

    let equal t1 t2 = compare t1 t2 = 0
  end

  module G = Graph.Persistent.Digraph.Concrete (V)

  type t = G.t

  let empty = G.empty
  let add_node = G.add_vertex
  let remove_node = G.remove_vertex
  let add_edge t ~src ~dst = G.add_edge t src dst
  let remove_edge t ~src ~dst = G.remove_edge t src dst
  let size = G.nb_vertex
  let size_edges = G.nb_edges
  let nodes t = G.fold_vertex List.cons t []
  let fold t ~init ~f = G.fold_vertex f t init
  let iter t ~f = G.iter_vertex f t
  let edges t = G.fold_edges (fun src dst edges -> (src, dst) :: edges) t []
  let fold_edges t ~init ~f = G.fold_edges (fun src dst edges -> f ~src ~dst edges) t init
  let iter_edges t ~f = G.iter_edges (fun src dst -> f ~src ~dst) t
  let mem = G.mem_vertex
  let mem_edge t ~src ~dst = G.mem_edge t src dst
  let succ = G.succ
  let pred = G.pred
  let map t ~f = G.map_vertex f t

  let remove_and_preserve_connectivity t node =
    let pred = pred t node
    and succ = succ t node in
    List.cartesian_product pred succ
    |> List.fold_left ~init:(remove_node t node) ~f:(fun t (src, dst) ->
           add_edge t ~src ~dst)
  ;;

  let update t ~from:node ~to_:node' =
    let pred = pred t node
    and succ = succ t node in
    let t = add_node (remove_node t node) node' in
    (* Add edges *)
    let t = List.fold_left ~init:t pred ~f:(fun t src -> add_edge t ~src ~dst:node') in
    let t = List.fold_left ~init:t succ ~f:(fun t dst -> add_edge t ~src:node' ~dst) in
    t
  ;;

  let update_many t ~from:node ~to_:nodes =
    let pred = pred t node
    and succ = succ t node in
    let t = remove_node t node in
    let hd = List.hd_exn nodes
    and last = List.last_exn nodes in
    (* Add nodes *)
    let t = List.fold_left nodes ~init:t ~f:add_node in
    (* Add edges *)
    let t = List.fold_left pred ~init:t ~f:(fun t src -> add_edge t ~src ~dst:hd) in
    let t =
      fst
      @@ List.fold_left (List.tl_exn nodes) ~init:(t, hd) ~f:(fun (t, src) dst ->
             add_edge t ~src ~dst, dst)
    in
    let t = List.fold_left succ ~init:t ~f:(fun t dst -> add_edge t ~src:last ~dst) in
    t
  ;;

  let mapf t ~f = fold t ~init:t ~f:(fun node t -> update t ~from:node ~to_:(f t node))

  let filter_map t ~f =
    fold t ~init:t ~f:(fun node t ->
        match f node with
        | Some node' -> update t ~from:node ~to_:node'
        | None -> remove_and_preserve_connectivity t node)
  ;;

  let map_many t ~f =
    fold t ~init:t ~f:(fun node t ->
        match f node with
        | [] -> raise_s [%message "[Flowgraph.map_many] Cannot map to no nodes!"]
        | nodes -> update_many t ~from:node ~to_:nodes)
  ;;

  let to_dot t ~label =
    let module Dot =
      Graph.Graphviz.Dot (struct
        include G

        let edge_attributes _ = [ `Color 4711 ]
        let default_edge_attributes _ = []
        let get_subgraph _ = None
        let vertex_attributes node = [ `Label (label node) ]
        let vertex_name node = Int.to_string (Node.hash node)
        let default_vertex_attributes _ = [ `Shape `Box ]
        let graph_attributes _ = []
      end)
    in
    let module Format = Caml.Format in
    Dot.fprint_graph Format.str_formatter t;
    Format.flush_str_formatter ()
  ;;

  module To_ocamlgraph = G
end
