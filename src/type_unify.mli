open Base
open Typed_expr

type substitution = (string * expr_type) list
type typing_error

val string_of_typing_error : typing_error -> string

val apply : substitution -> expr_type -> expr_type

val unify : (expr_type * expr_type) list -> (substitution, typing_error) Result.t

val unify_one : expr_type -> expr_type -> (substitution, typing_error) Result.t
