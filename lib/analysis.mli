open Core
module Ir := Ir.Simple
open Ir

val unreachable_nodes : Ir.t -> Ir.Node.t list

module Dataflow : sig
  type ('a, 'cmp) node =
    { in_ : ('a, 'cmp) Set.t
    ; out : ('a, 'cmp) Set.t
    }

  type ('a, 'cmp) t = Node.t -> ('a, 'cmp) node

  val analysis
    :  Ir.t
    -> init:('a, 'cmp) t
    -> f:(Node.t -> ('a, 'cmp) t -> ('a, 'cmp) node)
    -> ('a, 'cmp) t

  val to_dot : Ir.t -> ('a, _) t -> sexp_of_a:('a -> Sexp.t) -> string
end

open Instruction

module Liveness : sig
  type live = (Var.t, Var.comparator_witness) Dataflow.t

  val analysis : Ir.t -> live
end

module Avail : sig
  type avail = (Expr.t, Expr.comparator_witness) Dataflow.t

  val analysis : Ir.t -> avail
end

module Reach : sig
  type reach = (Node.t, Node.comparator_witness) Dataflow.t

  val analysis : Ir.t -> reach
end

module Copy : sig
  type t =
    { lhs : variable
    ; rhs : variable
    }
  [@@deriving compare, sexp, hash]

  include Comparable.S with type t := t
end

module Copy_reach : sig
  type copy_reach = (Copy.t, Copy.comparator_witness) Dataflow.t

  val analysis : Ir.t -> copy_reach
end
