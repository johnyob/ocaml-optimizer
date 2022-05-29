open Core

(** Labels, used to name blocks. *)
module Label : sig
  type t = string [@@deriving compare, sexp, hash]

  include Identifiable.S with type t := t
end

type label = Label.t [@@deriving sexp]

(* [fresh_label ()] returns a procedurally generated label. *)
val fresh_label : unit -> label

(** Variables or registers. *)
type variable = string [@@deriving compare, hash, sexp]

type expression =
  | Var of variable
  | Int of int
  | Plus of expression * expression
[@@deriving sexp]

type condition =
  | Equal of expression * expression
  | Greater_than of expression * expression
  | Less_than of expression * expression
  | Not_equal of expression * expression
[@@deriving sexp]

type jump_instruction =
  [ `Call of label * expression list
  | `Calli of variable * expression list
  | `Jump of label
  | `Jump_indir of variable
  | `Cond of condition * label
  | `Ret
  ]
[@@deriving sexp]

type basic_instruction =
  [ `Assign of variable * expression
  | `Address_of of variable * [ `Label of label | `Var of variable ]
  | `Deref of variable * expression
  | `Store of expression * variable
  ]
[@@deriving sexp]

type instruction =
  [ basic_instruction
  | jump_instruction
  | `Label of label
  ]
[@@deriving sexp]

type 'desc addressed =
  { addr : int
  ; desc : 'desc
  }
[@@deriving sexp]

type program = instruction list [@@deriving sexp]

val free_vars_expr : expression -> String.Set.t
val free_vars_cond : condition -> String.Set.t
val free_vars_instr : instruction -> String.Set.t
val def : instruction -> String.Set.t
val ref_ : instruction -> address_taken_vars:String.Set.t -> String.Set.t
val subst_expr : expression -> subst:(variable -> variable) -> expression
val subst_cond : condition -> subst:(variable -> variable) -> condition
val subst_instr_ref : instruction -> subst:(variable -> variable) -> instruction
val subst_instr_def : instruction -> subst:(variable -> variable) -> instruction