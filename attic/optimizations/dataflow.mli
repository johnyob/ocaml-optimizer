(* open Core
open Instruction
module Ir := Ir.Simple
open Ir

module Node : sig
  include module type of Node
  include Comparable.S with type t := t
end

type ('a, 'cmp) dataflow =
  { in_ : ('a, 'cmp) Set.t
  ; out : ('a, 'cmp) Set.t
  }

type ('a, 'cmp) result = Node.t -> ('a, 'cmp) dataflow

val analysis
  :  Ir.t
  -> init:('a, 'cmp) result
  -> f:(Node.t -> ('a, 'cmp) result -> ('a, 'cmp) dataflow)
  -> ('a, 'cmp) result

val to_dot : Ir.t -> ('a, _) result -> sexp_of_a:('a -> Sexp.t) -> string

module Liveness : sig
  type live = Node.t -> (string, String.comparator_witness) dataflow

  val analysis : Ir.t -> live
end

module Avail : sig
  open Instruction

  module Expr : sig
    type t = expression [@@deriving compare, hash, sexp]

    include Comparable.S with type t := t
  end

  type avail = Node.t -> (Expr.t, Expr.comparator_witness) dataflow

  val analysis : Ir.t -> avail
end

module Reach : sig
  type reach = Node.t -> (Node.t, Node.comparator_witness) dataflow

  val analysis : Ir.t -> reach
end

module Copy_reach : sig
  module Copy : sig
    type t =
      { lhs : variable
      ; rhs : variable
      }
    [@@deriving compare, sexp, hash]

    include Comparable.S with type t := t
  end

  type copy_reach = Node.t -> (Copy.t, Copy.comparator_witness) dataflow
end

module Transform : sig
  type t = Ir.t -> Ir.t

  val dead_code_elim : t
  val common_subexpr_elim : t
  val copy_prop : t
  val const_prop : t
end *)
