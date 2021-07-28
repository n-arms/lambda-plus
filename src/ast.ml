open Base

type var_name = int
type type_var_name = int

type op = 
    | UnaryMinus
    | Sub
    | Add
    | Div
    | Mul
    | Mod
    | Fix

type expr =
    | Num of int
    | App of expr * expr
    | Func of var_name * expr
    | Arg of var_name
    | Op of op
    | Let of var_name * expr * expr
    | LetRec of var_name * expr * expr

type mono_type =
    | TVar of type_var_name
    | TInt
    | TFun of (mono_type * mono_type)

type tvset = Set.M(Int).t

type poly_type =
    | Mono of mono_type
    | Poly of (tvset * mono_type)

let string_of_op = function
    | UnaryMinus -> "~"
    | Sub -> "-"
    | Add -> "+"
    | Div -> "/"
    | Mul -> "*"
    | Mod -> "%"
    | Fix -> "fix"

let string_of_var_name vn = Int.to_string vn

let rec string_of_expr = function
    | Num n -> Int.to_string n
    | App (l, r) -> "(" ^ (string_of_expr l) ^ " " ^ (string_of_expr r) ^ ")"
    | Func (a, e) -> "(\\" ^ (string_of_var_name a) ^ " -> " ^ (string_of_expr e) ^ ")"
    | Arg a -> string_of_var_name a
    | Op o -> string_of_op o
    | Let (a, e, b) -> "let " ^ (string_of_var_name a) ^ " = " ^ (string_of_expr e) ^ " in " ^ (string_of_expr b)
    | LetRec (a, e, b) -> "let rec " ^ (string_of_var_name a) ^ " = " ^ (string_of_expr e) ^ " in " ^ (string_of_expr b)

let rec string_of_mono_type = function
    | TVar t -> Int.to_string t
    | TInt -> "Int"
    | TFun (m, n) -> "(" ^ (string_of_mono_type m) ^ " -> " ^ (string_of_mono_type n) ^ ")"

let rec decode_arg i =
    if i < 26 then (i+97) |> Char.of_int_exn |> String.of_char else (122 |> Char.of_int_exn |> String.of_char)^(decode_arg (i - 25))

let rec encode_arg s =
    String.fold s ~init:0 ~f:(fun acc c -> acc + ((Char.to_int c) - 97))

let rec string_of_poly_type = 
    let rec join_with_commas f = function
        | h1::h2::tl -> (f h1)^", "^(join_with_commas f (h2::tl))
        | [hd] -> (f hd)
        | [] -> "" in
    function
    | Mono m -> "forall [] "^(string_of_mono_type m)
    | Poly (tvns, m) -> "forall ["^(Set.to_list tvns |> join_with_commas decode_arg )^"] "^(string_of_mono_type m)
    
