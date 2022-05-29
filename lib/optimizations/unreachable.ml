open Core
module Ir = Ir.Simple



let unreachable_nodes =
  let open Ir in
  fun { flowgraph; entry } ->
    let visited = Hash_set.create (module Ir.Node) in
    (* Traverse the flowgraph [flowgraph] *)
    let rec loop node =
      (* print_endline () *)
      if not (Hash_set.mem visited node)
      then (
        (* Visit node *)
        Hash_set.add visited node;
        (* Visit children/successors *)
        Flowgraph.succ flowgraph node |> List.iter ~f:loop)
    in
    loop entry;
    Flowgraph.nodes flowgraph |> List.filter ~f:(fun node -> not (Hash_set.mem visited node))
;;

let eliminate =
  let open Ir in
  fun ir ->
    (* Compute the unreachable nodes in the flowgraph *)
    let unreachable_nodes = unreachable_nodes ir in
    (* Remove unreachable nodes *)
    let flowgraph =
      List.fold_left unreachable_nodes ~init:ir.flowgraph ~f:Flowgraph.remove_node
    in
    { ir with flowgraph }
;;
