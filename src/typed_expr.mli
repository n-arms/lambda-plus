open Base

type typing_error

type expr_type =
    | TEInt
    | TEGeneric
    | TEFunc of expr_type * expr_type

type typed_expr =
    | Num of int
    | App of expr_type * Expr.expr * Expr.expr
    | Func of expr_type * expr_type * string * Expr.expr
    | Arg of expr_type * string
    | Op of Expr.op

val string_of_typed_expr : typed_expr -> string
val string_of_typing_error : typing_error -> string
val string_of_expr_type : expr_type -> string

val get_type : Expr.expr -> (string, expr_type, 'a) Map.t -> (expr_type, typing_error) result

