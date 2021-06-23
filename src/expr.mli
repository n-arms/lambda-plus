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

val string_of_expr : expr -> string
val string_of_op : op -> string
