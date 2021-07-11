open Base
open Expr

type expr_type =
    | TEInt
    | TEGeneric of string
    | TEArrow of expr_type * expr_type

let rec string_of_expr_type = function
    | TEInt -> "Num"
    | TEGeneric s -> "'"^s
    | TEArrow (t1, t2) -> 
            "("^
            (string_of_expr_type t1)^
            " -> "^
            (string_of_expr_type t2)^
            ")"

type typed_expr =
    | ANum of int
    | AApp of typed_expr * typed_expr * expr_type
    | AFunc of string * typed_expr * expr_type
    | AArg of string * expr_type
    | AOp of op

let rec string_of_typed_expr = function
    | ANum n -> "("^(let open Int in to_string n)^")"
    | AApp (e1, e2, t) -> "("^(string_of_typed_expr e1)^" "^(string_of_typed_expr e2)^")::"^(string_of_expr_type t)
    | AFunc (s, e, t) -> "(\\"^s^" -> "^(string_of_typed_expr e)^")::"^(string_of_expr_type t)
    | AArg (s, t) -> "("^s^"::"^(string_of_expr_type t)^")"
    | AOp o -> string_of_op o
