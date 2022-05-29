open Core
module Label : Identifiable.S with type t = string = String

type label = Label.t [@@deriving sexp]
type variable = string [@@deriving compare, hash, sexp]

let fresh_label =
  let next = ref 0 in
  fun () ->
    incr next;
    "label_" ^ Int.to_string !next
;;

(* 
x := e
if cond then goto label
lab(e1, .., en)
[x](e1, ..., en)
goto label
goto [x]
ret

(* gets address of x or lab and stores in x *)
x := &(x|lab)

(* load value pointed to by e into x *)
x := [e]
def = { x }
ref = all address taken variables \cup ref(e)

(* computes e, stores in location pointed to by x *)
[x] := e
def = emptyset
ref = ref(e) \cup { x }
*)

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

let def instr =
  String.Set.(
    match instr with
    | `Assign (x, _) | `Address_of (x, _) | `Deref (x, _) -> singleton x
    | _ -> empty)
;;

let rec free_vars_expr expr =
  String.Set.(
    match expr with
    | Var x -> singleton x
    | Int _ -> empty
    | Plus (expr1, expr2) -> union (free_vars_expr expr1) (free_vars_expr expr2))
;;

let free_vars_cond (cond : condition) =
  match cond with
  | Equal (expr1, expr2)
  | Greater_than (expr1, expr2)
  | Less_than (expr1, expr2)
  | Not_equal (expr1, expr2) -> Set.union (free_vars_expr expr1) (free_vars_expr expr2)
;;

let free_vars_instr instr =
  String.Set.(
    match instr with
    | `Assign (x, expr) -> add (free_vars_expr expr) x
    | `Address_of (x1, `Var x2) -> of_list [ x1; x2 ]
    | `Deref (x, expr) -> add (free_vars_expr expr) x
    | `Store (expr, x) -> add (free_vars_expr expr) x
    | `Call (_, exprs) -> exprs |> List.map ~f:free_vars_expr |> union_list
    | `Calli (x, exprs) -> add (exprs |> List.map ~f:free_vars_expr |> union_list) x
    | `Jump_indir x -> singleton x
    | `Cond (cond, _) -> free_vars_cond cond
    | _ -> empty)
;;

let ref_ instr ~address_taken_vars =
  String.Set.(
    match instr with
    | `Assign (_, expr) -> free_vars_expr expr
    | `Cond (cond, _) -> free_vars_cond cond
    | `Call (_, exprs) -> union_list (List.map exprs ~f:free_vars_expr)
    | `Jump_indir x -> singleton x
    | `Calli (x, exprs) -> add (union_list (List.map exprs ~f:free_vars_expr)) x
    | `Deref (x, expr) -> union (add address_taken_vars x) (free_vars_expr expr)
    | `Store (expr, x) -> add (free_vars_expr expr) x
    | _ -> empty)
;;

let rec subst_expr expr ~subst =
  match expr with
  | Var x -> Var (subst x)
  | Int x -> Int x
  | Plus (expr1, expr2) -> Plus (subst_expr ~subst expr1, subst_expr ~subst expr2)
;;

let subst_cond (cond : condition) ~subst : condition =
  match cond with
  | Equal (expr1, expr2) -> Equal (subst_expr ~subst expr1, subst_expr ~subst expr2)
  | Greater_than (expr1, expr2) ->
    Greater_than (subst_expr ~subst expr1, subst_expr ~subst expr2)
  | Less_than (expr1, expr2) ->
    Less_than (subst_expr ~subst expr1, subst_expr ~subst expr2)
  | Not_equal (expr1, expr2) ->
    Not_equal (subst_expr ~subst expr1, subst_expr ~subst expr2)
;;

let subst_instr_ref instr ~subst =
  match instr with
  | `Call (lab, exprs) -> `Call (lab, List.map exprs ~f:(subst_expr ~subst))
  | `Calli (x, exprs) -> `Calli (subst x, List.map exprs ~f:(subst_expr ~subst))
  | `Jump label -> `Jump label
  | `Jump_indir x -> `Jump_indir (subst x)
  | `Cond (cond, label) -> `Cond (subst_cond ~subst cond, label)
  | `Ret -> `Ret
  | `Assign (x, expr) -> `Assign (x, subst_expr ~subst expr)
  | `Address_of (x, rhs) ->
    `Address_of
      ( x
      , match rhs with
        | `Label label -> `Label label
        | `Var x -> `Var (subst x) )
  | `Deref (x, expr) -> `Deref (x, subst_expr ~subst expr)
  | `Store (expr, x) -> `Store (subst_expr ~subst expr, subst x)
  | `Label label -> `Label label
;;

let subst_instr_def instr ~subst =
  match instr with
  | `Assign (x, expr) -> `Assign (subst x, expr)
  | `Address_of (x, rhs) -> `Address_of (subst x, rhs)
  | `Deref (x, expr) -> `Deref (subst x, expr)
  | instr -> instr
;;
