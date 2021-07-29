type op =
    | Arrow
    | Equals
    | Times
    | Plus
    | Minus
    | UnaryMinus
    | Slash
    | Lambda 
    | Let
    | In
    | Rec
    | Fix

type token =
    | Op of op
    | Arg of string
    | Num of string
    | Bool of bool
    | LPar
    | RPar

val lex : string -> token list
val to_string : token -> string
