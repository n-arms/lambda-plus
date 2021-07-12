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

type token =
    | Op of op
    | Arg of string
    | Num of string
    | LPar
    | RPar

val lex : string -> token list
val to_string : token -> string
