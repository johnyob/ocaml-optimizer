open Core

(** Labels, used to name blocks. *)
module Label : sig
  type t = string [@@deriving compare, sexp, hash]

  val fresh : unit -> t

  include Identifiable.S with type t := t
end

(** Variables/registers *)
module Var : sig
  type t = string [@@deriving compare, sexp, hash]

  val fresh : ?prefix:string -> unit -> t

  include Identifiable.S with type t := t
end

(** Simple expressions *)
module Expr : sig
  type t =
    | Var of Var.t
    | Int of int
    | Plus of t * t
    | Mul of t * t
    | Sub of t * t
  [@@deriving compare, sexp]

  include Comparable.S with type t := t

  val free_vars : t -> Var.Set.t
  val sub_exprs : t -> Set.t
  val size : t -> int
  val subst_var : t -> subst:(Var.t -> Var.t) -> t
  val subst_expr : t -> subst:(Var.t -> t) -> t
  val is_const : t -> bool
  val eval_const_expr : t -> int
  val simplify : t -> t

  val to_string : t -> string
end

(** Jump conditions *)
module Condition : sig
  type t =
    | Equal of Expr.t * Expr.t
    | Greater_than of Expr.t * Expr.t
    | Less_than of Expr.t * Expr.t
    | Not_equal of Expr.t * Expr.t
  [@@deriving compare, sexp]

  include Comparable.S with type t := t

  val free_vars : t -> Var.Set.t
  val exprs : t -> Expr.t list
  val subst_var : t -> subst:(Var.t -> Var.t) -> t
  val subst_expr : t -> subst:(Var.t -> Expr.t) -> t
  val is_const : t -> bool
  val eval_const_cond : t -> bool

  val to_string : t -> string
end

type label = Label.t [@@deriving sexp]
type variable = Var.t [@@deriving compare, hash, sexp]
type expression = Expr.t [@@deriving compare, sexp]
type condition = Condition.t [@@deriving compare, sexp]

type jump_instruction =
  [ `Call of label * expression list
  | `Calli of variable * expression list
  | `Jump of label
  | `Jump_indir of variable
  | `Cond of condition * label
  | `Ret
  ]
[@@deriving compare, sexp]

type basic_instruction =
  [ `Assign of variable * expression
  | `Address_of of variable * [ `Label of label | `Var of variable ]
  | `Deref of variable * expression
  | `Store of expression * variable
  ]
[@@deriving compare, sexp]

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
[@@deriving compare, sexp]

type program = instruction list [@@deriving compare, sexp]

val free_vars_all : instruction -> Var.Set.t
val free_vars_ref : instruction -> Var.Set.t
val free_vars_def : instruction -> Var.Set.t
val exprs : instruction -> expression list
val def : instruction -> Var.Set.t
val ref_ : instruction -> address_taken_vars:Var.Set.t -> Var.Set.t
val subst_ref_var : instruction -> subst:(variable -> variable) -> instruction
val subst_def_var : instruction -> subst:(variable -> variable) -> instruction
val subst_expr : instruction -> subst:(variable -> expression) -> instruction
val is_const_assign : instruction -> (variable * int) option

val to_string : instruction -> string