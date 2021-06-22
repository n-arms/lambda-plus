open Base

type t
type parseError
type arg
type num
type app
type func
type op
type expr =
    | Num of num
    | App of app
    | Func of func
    | Arg of arg
    | Op of op
type 'a parser = t -> ('a, parseError) result*t


val chars : char list -> char list parser
val make : string -> t
val to_string : t -> string
val from_string : string -> char list
val from_char_list : char list -> string
val from_parse_error : parseError -> string
val from_expr : expr -> string

val (>>=) : 'a parser -> ('a -> 'b parser) -> 'b parser
val (>>|) : 'a parser -> ('a -> 'b) -> 'b parser
val (<*) : 'a parser -> 'b parser -> 'a parser
val ( *> ) : 'a parser -> 'b parser -> 'b parser
val many : 'a parser -> 'a list parser
val many1 : 'a parser -> 'a list parser
val select : (char -> bool) -> char parser

val parse_expr : expr parser
