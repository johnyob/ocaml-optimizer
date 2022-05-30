module Ir := Ir.Simple

type t = Ir.t -> continue:(unit -> unit) -> Ir.t

val fix : f:t -> t

val unreachable_elim : t
val dead_code_elim : t
val common_subexpr_elim : t
val copy_prop : t
val const_prop : t

val peephole_data : t
val peephole_control : t

val all : t list -> t
val single : t -> Ir.t -> Ir.t
val aggressive : Ir.t -> Ir.t
