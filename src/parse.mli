open Base
open Ast

type t
type parseError
type 'a parser = t -> ('a, parseError) Result.t*t

val make : Lex.token list -> t
val to_string : t -> string
val from_parse_error : parseError -> string

val (>>=) : 'a parser -> ('a -> 'b parser) -> 'b parser
val (>>|) : 'a parser -> ('a -> 'b) -> 'b parser
val (<*) : 'a parser -> 'b parser -> 'a parser
val ( *> ) : 'a parser -> 'b parser -> 'b parser
val many : 'a parser -> 'a list parser
val many1 : 'a parser -> 'a list parser
val select : (Lex.token -> bool) -> Lex.token parser

val parse_expr : expr parser
