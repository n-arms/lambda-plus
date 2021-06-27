open Base

type expr_type =
    | TEInt
    | TEGeneric of int
    | TEFunc of expr_type * expr_type

let string_of_typing_error e = e

let rec string_of_expr_type = function
    | TEInt -> "Num"
    | TEGeneric i -> "<gen "^(let open Int in to_string i)^">"
    | TEFunc (t1, t2) -> 
            "("^
            (string_of_expr_type t1)^
            " -> "^
            (string_of_expr_type t2)^
            ")"
