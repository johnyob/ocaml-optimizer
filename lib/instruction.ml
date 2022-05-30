open Core

module Label = struct
  include String

  let fresh =
    let next = ref 0 in
    fun () ->
      incr next;
      "#lab_" ^ Int.to_string !next
  ;;
end

module Var = struct
  include String

  let fresh =
    let next = ref 0 in
    fun ?(prefix = "") () ->
      incr next;
      prefix ^ Int.to_string !next
  ;;
end

module Expr = struct
  module T = struct
    type t =
      | Var of Var.t
      | Int of int
      | Plus of t * t
      | Mul of t * t
      | Sub of t * t
    [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)

  let rec free_vars expr =
    Var.Set.(
      match expr with
      | Var x -> singleton x
      | Int _ -> empty
      | Plus (expr1, expr2) | Mul (expr1, expr2) | Sub (expr1, expr2) ->
        union (free_vars expr1) (free_vars expr2))
  ;;

  let rec sub_exprs expr =
    match expr with
    | Var _ | Int _ -> Set.empty
    | Plus ((Var _ | Int _), (Var _ | Int _)) -> Set.singleton expr
    | Plus (expr1, expr2) | Mul (expr1, expr2) | Sub (expr1, expr2) ->
      Set.add (Set.union (sub_exprs expr1) (sub_exprs expr2)) expr
  ;;

  let rec size expr =
    match expr with
    | Var _ | Int _ -> 1
    | Plus (expr1, expr2) | Mul (expr1, expr2) | Sub (expr1, expr2) ->
      1 + size expr1 + size expr2
  ;;

  let rec subst_var expr ~subst =
    match expr with
    | Var x -> Var (subst x)
    | Int x -> Int x
    | Plus (expr1, expr2) -> Plus (subst_var ~subst expr1, subst_var ~subst expr2)
    | Mul (expr1, expr2) -> Mul (subst_var ~subst expr1, subst_var ~subst expr2)
    | Sub (expr1, expr2) -> Sub (subst_var ~subst expr1, subst_var ~subst expr2)
  ;;

  let rec subst_expr expr ~subst =
    match expr with
    | Var x -> subst x
    | Int x -> Int x
    | Plus (expr1, expr2) -> Plus (subst_expr ~subst expr1, subst_expr ~subst expr2)
    | Mul (expr1, expr2) -> Mul (subst_expr ~subst expr1, subst_expr ~subst expr2)
    | Sub (expr1, expr2) -> Sub (subst_expr ~subst expr1, subst_expr ~subst expr2)
  ;;

  let rec is_const expr =
    match expr with
    | Var _ -> false
    | Int _ -> true
    | Plus (expr1, expr2) | Mul (expr1, expr2) | Sub (expr1, expr2) ->
      is_const expr1 && is_const expr2
  ;;

  let rec eval_const_expr expr =
    match expr with
    | Var _ -> assert false
    | Int n -> n
    | Plus (expr1, expr2) -> eval_const_expr expr1 + eval_const_expr expr2
    | Mul (expr1, expr2) -> eval_const_expr expr1 * eval_const_expr expr2
    | Sub (expr1, expr2) -> eval_const_expr expr1 - eval_const_expr expr2
  ;;

  let rec simplify expr =
    match expr with
    | Var x -> Var x
    | Int n -> Int n
    | Plus (expr1, expr2) ->
      let expr1 = simplify expr1
      and expr2 = simplify expr2 in
      (match expr1, expr2 with
      | expr, Int 0 | Int 0, expr -> expr
      | (Plus (expr, Int m) | Plus (Int m, expr)), Int n
      | Int n, (Plus (expr, Int m) | Plus (Int m, expr)) -> Plus (expr, Int (m + n))
      | expr1, expr2 -> Plus (expr1, expr2))
    | Sub (expr1, expr2) ->
      let expr1 = simplify expr1
      and expr2 = simplify expr2 in
      (match expr1, expr2 with
      | expr, Int 0 | Int 0, expr -> expr
      | expr1, expr2 -> Sub (expr1, expr2))
    | Mul (expr1, expr2) ->
      let expr1 = simplify expr1
      and expr2 = simplify expr2 in
      (match expr1, expr2 with
      | expr, Int 1 | Int 1, expr -> expr
      | (Mul (expr, Int m) | Mul (Int m, expr)), Int n
      | Int n, (Mul (expr, Int m) | Mul (Int m, expr)) -> Sub (expr, Int (m * n))
      | expr1, expr2 -> Mul (expr1, expr2))
  ;;
end

module Condition = struct
  module T = struct
    type t =
      | Equal of Expr.t * Expr.t
      | Greater_than of Expr.t * Expr.t
      | Less_than of Expr.t * Expr.t
      | Not_equal of Expr.t * Expr.t
    [@@deriving compare, sexp]
  end

  include T
  include Comparable.Make (T)

  let free_vars cond =
    match cond with
    | Equal (expr1, expr2)
    | Greater_than (expr1, expr2)
    | Less_than (expr1, expr2)
    | Not_equal (expr1, expr2) ->
      Var.Set.union (Expr.free_vars expr1) (Expr.free_vars expr2)
  ;;

  let exprs cond =
    match cond with
    | Equal (expr1, expr2)
    | Greater_than (expr1, expr2)
    | Less_than (expr1, expr2)
    | Not_equal (expr1, expr2) -> [ expr1; expr2 ]
  ;;

  let subst_var cond ~subst =
    match cond with
    | Equal (expr1, expr2) ->
      Equal (Expr.subst_var ~subst expr1, Expr.subst_var ~subst expr2)
    | Greater_than (expr1, expr2) ->
      Greater_than (Expr.subst_var ~subst expr1, Expr.subst_var ~subst expr2)
    | Less_than (expr1, expr2) ->
      Less_than (Expr.subst_var ~subst expr1, Expr.subst_var ~subst expr2)
    | Not_equal (expr1, expr2) ->
      Not_equal (Expr.subst_var ~subst expr1, Expr.subst_var ~subst expr2)
  ;;

  let subst_expr cond ~subst =
    match cond with
    | Equal (expr1, expr2) ->
      Equal (Expr.subst_expr ~subst expr1, Expr.subst_expr ~subst expr2)
    | Greater_than (expr1, expr2) ->
      Greater_than (Expr.subst_expr ~subst expr1, Expr.subst_expr ~subst expr2)
    | Less_than (expr1, expr2) ->
      Less_than (Expr.subst_expr ~subst expr1, Expr.subst_expr ~subst expr2)
    | Not_equal (expr1, expr2) ->
      Not_equal (Expr.subst_expr ~subst expr1, Expr.subst_expr ~subst expr2)
  ;;

  let is_const cond =
    match cond with
    | Equal (expr1, expr2)
    | Not_equal (expr1, expr2)
    | Greater_than (expr1, expr2)
    | Less_than (expr1, expr2) -> Expr.is_const expr1 && Expr.is_const expr2
  ;;

  let eval_const_cond cond =
    Int.(
      match cond with
      | Equal (expr1, expr2) -> Expr.eval_const_expr expr1 = Expr.eval_const_expr expr2
      | Not_equal (expr1, expr2) ->
        Expr.eval_const_expr expr1 <> Expr.eval_const_expr expr2
      | Greater_than (expr1, expr2) ->
        Expr.eval_const_expr expr1 > Expr.eval_const_expr expr2
      | Less_than (expr1, expr2) ->
        Expr.eval_const_expr expr1 < Expr.eval_const_expr expr2)
  ;;
end

type label = Label.t [@@deriving compare, hash, sexp]
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
[@@deriving compare, sexp]

type 'desc addressed =
  { addr : int
  ; desc : 'desc
  }
[@@deriving compare, sexp]

type program = instruction list [@@deriving compare, sexp]

let free_vars_ref (instr : instruction) =
  Var.Set.(
    match instr with
    | `Assign (_, expr) -> Expr.free_vars expr
    | `Address_of (_, `Var x) -> singleton x
    | `Deref (_, expr) -> Expr.free_vars expr
    | `Store (expr, x) -> add (Expr.free_vars expr) x
    | `Call (_, exprs) -> exprs |> List.map ~f:Expr.free_vars |> union_list
    | `Calli (x, exprs) -> add (exprs |> List.map ~f:Expr.free_vars |> union_list) x
    | `Jump_indir x -> singleton x
    | `Cond (cond, _) -> Condition.free_vars cond
    | _ -> empty)
;;

let free_vars_def (instr : instruction) =
  Var.Set.(
    match instr with
    | `Assign (x, _) | `Address_of (x, _) | `Deref (x, _) -> singleton x
    | _ -> empty)
;;

let free_vars_all (instr : instruction) =
  Var.Set.(
    match instr with
    | `Assign (x, expr) -> add (Expr.free_vars expr) x
    | `Address_of (x1, `Var x2) -> of_list [ x1; x2 ]
    | `Deref (x, expr) -> add (Expr.free_vars expr) x
    | `Store (expr, x) -> add (Expr.free_vars expr) x
    | `Call (_, exprs) -> exprs |> List.map ~f:Expr.free_vars |> union_list
    | `Calli (x, exprs) -> add (exprs |> List.map ~f:Expr.free_vars |> union_list) x
    | `Jump_indir x -> singleton x
    | `Cond (cond, _) -> Condition.free_vars cond
    | _ -> empty)
;;

let exprs instr =
  match instr with
  | `Assign (_, expr) | `Deref (_, expr) | `Store (expr, _) -> [ expr ]
  | `Call (_, exprs) | `Calli (_, exprs) -> exprs
  | `Cond (cond, _) -> Condition.exprs cond
  | _ -> []
;;

let def instr =
  String.Set.(
    match instr with
    | `Assign (x, _) | `Address_of (x, _) | `Deref (x, _) -> singleton x
    | _ -> empty)
;;

let ref_ (instr : instruction) ~address_taken_vars =
  Var.Set.(
    match instr with
    | `Assign (_, expr) -> Expr.free_vars expr
    | `Cond (cond, _) -> Condition.free_vars cond
    | `Call (_, exprs) -> union_list (List.map exprs ~f:Expr.free_vars)
    | `Jump_indir x -> singleton x
    | `Calli (x, exprs) -> add (union_list (List.map exprs ~f:Expr.free_vars)) x
    | `Deref (x, expr) -> union (add address_taken_vars x) (Expr.free_vars expr)
    | `Store (expr, x) -> add (Expr.free_vars expr) x
    | _ -> empty)
;;

let subst_ref_var instr ~subst =
  match instr with
  | `Call (lab, exprs) -> `Call (lab, List.map exprs ~f:(Expr.subst_var ~subst))
  | `Calli (x, exprs) -> `Calli (subst x, List.map exprs ~f:(Expr.subst_var ~subst))
  | `Jump label -> `Jump label
  | `Jump_indir x -> `Jump_indir (subst x)
  | `Cond (cond, label) -> `Cond (Condition.subst_var ~subst cond, label)
  | `Ret -> `Ret
  | `Assign (x, expr) -> `Assign (x, Expr.subst_var ~subst expr)
  | `Address_of (x, rhs) ->
    `Address_of
      ( x
      , match rhs with
        | `Label label -> `Label label
        | `Var x -> `Var (subst x) )
  | `Deref (x, expr) -> `Deref (x, Expr.subst_var ~subst expr)
  | `Store (expr, x) -> `Store (Expr.subst_var ~subst expr, subst x)
  | `Label label -> `Label label
;;

let subst_def_var instr ~subst =
  match instr with
  | `Assign (x, expr) -> `Assign (subst x, expr)
  | `Address_of (x, rhs) -> `Address_of (subst x, rhs)
  | `Deref (x, expr) -> `Deref (subst x, expr)
  | instr -> instr
;;

let subst_expr instr ~subst =
  match instr with
  | `Call (lab, exprs) -> `Call (lab, List.map exprs ~f:(Expr.subst_expr ~subst))
  | `Calli (x, exprs) -> `Calli (x, List.map exprs ~f:(Expr.subst_expr ~subst))
  | `Jump label -> `Jump label
  | `Jump_indir x -> `Jump_indir x
  | `Cond (cond, label) -> `Cond (Condition.subst_expr ~subst cond, label)
  | `Ret -> `Ret
  | `Assign (x, expr) -> `Assign (x, Expr.subst_expr ~subst expr)
  | `Address_of (x, rhs) -> `Address_of (x, rhs)
  | `Deref (x, expr) -> `Deref (x, Expr.subst_expr ~subst expr)
  | `Store (expr, x) -> `Store (Expr.subst_expr ~subst expr, x)
  | `Label label -> `Label label
;;

let is_const_assign instr =
  match instr with
  | `Assign (x, expr) when Expr.is_const expr -> Some (x, Expr.eval_const_expr expr)
  | _ -> None
;;