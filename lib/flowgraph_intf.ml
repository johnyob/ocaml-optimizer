module type S = sig
  type node

  (** [t] is the type of the flowgraph *)
  type t

  (** [empty] is the empty flowgraph *)
  val empty : t

  (** [add_node t node] adds basic node [node] to flowgraph [t] *)
  val add_node : t -> node -> t

  (** [remove t node] removes [node] from [t] *)
  val remove_node : t -> node -> t

  (** [add_edge t ~src ~dst] adds edge between [src] and [dst] to [t]. *)
  val add_edge : t -> src:node -> dst:node -> t

  (** [remove_edge t ~src ~dst] removes edge between [src] and [dst]. *)
  val remove_edge : t -> src:node -> dst:node -> t

  (** [size t] returns number of nodes in [t]. *)
  val size : t -> int

  (** [size_edges t] returns number of edges in [t]. *)
  val size_edges : t -> int

  (** [nodes t] returns nodes of [t] *)
  val nodes : t -> node list

  val fold : t -> init:'a -> f:(node -> 'a -> 'a) -> 'a
  val iter : t -> f:(node -> unit) -> unit

  (** [mem t node] is true if [node] is in [t] *)
  val mem : t -> node -> bool

  (** [edges t] returns edges of [t] *)
  val edges : t -> (node * node) list

  val fold_edges : t -> init:'a -> f:(src:node -> dst:node -> 'a -> 'a) -> 'a
  val iter_edges : t -> f:(src:node -> dst:node -> unit) -> unit

  (** [mem_edge t ~src ~dst] if there is an edge [src, dst] in [t]. *)
  val mem_edge : t -> src:node -> dst:node -> bool

  (** [succ t node] returns the successors of [t]. *)
  val succ : t -> node -> node list

  (** [pred t node] returns the predecessors of [t]. *)
  val pred : t -> node -> node list

  (** [to_dot t ~label] returns dot encoding of [t] *)
  val to_dot : t -> label:(node -> string) -> string

  module To_ocamlgraph :
    Graph.Sig.P with type t = t and type V.t = node and type E.t = node * node
end

module type Node = sig
  type t [@@deriving sexp, compare, hash]
end

module type Intf = sig
  module type S = S
  module type Node = Node

  module Make (Node : Node) : S with type node := Node.t
end
