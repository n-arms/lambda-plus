open Base

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

let to_string_op = function
    | Arrow -> "->"
    | Equals -> "="
    | Times -> "*"
    | Plus -> "+"
    | Minus -> "-"
    | UnaryMinus -> "~"
    | Slash -> "/"
    | Lambda -> "\\"
    | Let -> "let"
    | In -> "in"
    | Rec -> "rec"
    | Fix -> "fix"

type token =
    | Op of op
    | Arg of string
    | Num of string
    | LPar
    | RPar

let to_string = function
    | Op o -> to_string_op o
    | Arg a -> a
    | Num n -> n
    | LPar -> "("
    | RPar -> ")"

exception LexError of string

let lex s =
    let rec get_while p = function
        | hd::tl when p hd -> 
                let out, rest = get_while p tl 
                in (hd::out, rest)
        | l ->
                ([], l)
    in
    let rec lex' = function
        | '('::tl -> LPar::(lex' tl)
        | ')'::tl -> RPar::(lex' tl)
        | '-'::'>'::tl -> Op Arrow::(lex' tl)
        | '='::tl -> Op Equals::(lex' tl)
        | '*'::tl -> Op Times::(lex' tl)
        | '+'::tl -> Op Plus::(lex' tl)
        | '-'::tl -> Op Minus::(lex' tl)
        | '~'::tl -> Op UnaryMinus::(lex' tl)
        | '/'::tl -> Op Slash::(lex' tl)
        | '\\'::tl -> Op Lambda::(lex' tl)
        | ' '::tl | '\n'::tl -> lex' tl
        | 'l'::'e'::'t'::tl -> Op Let::(lex' tl)
        | 'i'::'n'::tl -> Op In::(lex' tl)
        | 'r'::'e'::'c'::tl -> Op Rec::(lex' tl)
        | 'f'::'i'::'x'::tl -> Op Fix::(lex' tl)
        | hd::tl when Char.is_digit hd -> 
                let out, rest = get_while Char.is_digit (hd::tl)
                in
                Num (List.fold out ~init:"" ~f:(fun s c -> s^(Char.to_string c)))::(lex' rest)
        | hd::tl -> 
                let out, rest = get_while Char.is_alpha (hd::tl)
                in 
                let a = (List.fold out ~init:"" ~f:(fun s c -> s^(let open Char in to_string c)))
                in if (String.length a) = 0 then raise (LexError ("illegal char "^(Char.to_string hd))) else Arg a :: (lex' rest)
        | [] -> []
    in
    let from_string s =
        let out = ref [] in
        for i = (let open String in length s) - 1 downto 0 do
            out := s.[i]::!out done;
            !out
    in lex' (from_string s)

