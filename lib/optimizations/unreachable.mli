module Ir := Ir.Simple

val unreachable_nodes : Ir.t -> Ir.Node.t list
val eliminate : Ir.t -> Ir.t
