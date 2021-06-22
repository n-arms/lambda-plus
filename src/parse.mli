open Base

type t
type parseError
type arg
type num
type app
type func
type expr =
    | Num of num
    | App of app
    | Func of func
    | Arg of arg


val chars : char list -> t -> bool*t
val make : string -> t
val to_string : t -> string
val from_string : string -> char list
val from_char_list : char list -> string
val from_parse_error : parseError -> string
val from_expr : expr -> string

val many : (t -> ('a, parseError) result*t) -> t -> ('a list, parseError) result*t
val select : (char -> bool) -> t -> (char, parseError) result*t

val parse_arg : t -> (arg, parseError) result*t
(*val parse_num : t -> (num, parseError) result*t*)
