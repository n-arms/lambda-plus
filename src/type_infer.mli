open Base
open Expr
open Typed_expr

type scope
val empty : scope

type typing_error
val string_of_typing_error : typing_error -> string

val infer_type : expr -> scope -> (expr_type * scope, typing_error) Result.t
