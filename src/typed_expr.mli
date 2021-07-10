open Base
open Expr

type expr_type =
    | TEInt
    | TEGeneric of string
    | TEArrow of expr_type * expr_type

val string_of_expr_type : expr_type -> string

type typed_expr =
    | ANum of int
    | AApp of typed_expr * typed_expr * expr_type
    | AFunc of string * typed_expr * expr_type
    | AArg of string * expr_type
    | AOp of op

val string_of_typed_expr : typed_expr -> string


