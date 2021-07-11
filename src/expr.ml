open Base

type op = 
    | UnaryMinus
    | Sub
    | Add
    | Div
    | Mul
    | Mod

type expr =
    | Num of int
    | App of expr * expr
    | Func of string * expr
    | Arg of string
    | Op of op
    | Let of string * expr * expr


let string_of_op = function
    | UnaryMinus -> "~"
    | Sub -> "-"
    | Add -> "+"
    | Div -> "/"
    | Mul -> "*"
    | Mod -> "%"
    
let rec string_of_expr = function
    | Num i -> 
            (let open Int in to_string i)
    | App (e1, e2) -> 
            "("^(string_of_expr e1)^" "^(string_of_expr e2)^")"
    | Func (s, e) -> "(\\"^s^" -> "^(string_of_expr e)^")"
    | Arg s -> s
    | Op o -> (string_of_op o)
    | Let (s, e1, e2) -> ("let "^s^" = "^(string_of_expr e1)^" in "^(string_of_expr e2))

