open Base

type expr_type =
    | TEInt
    | TEGeneric of int
    | TEFunc of expr_type * expr_type

val string_of_expr_type : expr_type -> string
