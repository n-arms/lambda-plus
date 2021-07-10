open Base
open Expr
open Typed_expr

type substitution = (string * expr_type) list

val apply : substitution -> expr_type -> expr_type

val unify : (expr_type * expr_type) list -> substitution

val unify_one : expr_type -> expr_type -> substitution
