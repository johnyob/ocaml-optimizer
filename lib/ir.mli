(* open Core *)
open Instruction
open Flowgraph

val fresh_id : unit -> int

module type S = sig
  module Node : Node
  module Flowgraph : Flowgraph.S with type node := Node.t

  type t =
    { entry : Node.t
    ; flowgraph : Flowgraph.t
    }

  val to_dot : t -> label:(Node.t -> string) -> string
end

module Simple : sig
  module Node : sig
    type t =
      { id : int
      ; instr : instruction
      }
    [@@deriving sexp, hash, compare]
  end

  include S with module Node := Node

  val of_program : program -> t
end

module Basic_block : sig
  module Node : sig
    type t =
      { id : int
      ; block : instruction list
      }
    [@@deriving sexp, hash, compare]
  end

  include S with module Node := Node

  val of_program : program -> t
  val of_simple : Simple.t -> t
end

module Ssa : sig
  module Node : sig
    type t =
      { id : int
      ; mutable phis : [ `Phi of variable * variable list ] list
      ; mutable block : instruction list
      }
    [@@deriving sexp, hash, compare]
  end

  include S with module Node := Node
  (* include Invariant.S with type t := t *)
(* 
  val of_program : program -> t
  val of_simple : Simple.t -> t *)
  val of_basic_block : Basic_block.t -> t
end