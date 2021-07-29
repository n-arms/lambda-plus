open Base
open Util

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

type lit =
    | Bool of bool
    | Int of int

type expr =
    | Lit of lit
    | App of expr * expr
    | Func of var_name * expr
    | Arg of var_name
    | Op of op
    | Let of var_name * expr * expr
    | LetRec of var_name * expr * expr
    | Tuple of expr list

type prim =
    | PBool
    | PInt

type mono_type =
    | TVar of type_var_name
    | TFun of (mono_type * mono_type)
    | TPrim of prim
    | TProd of mono_type list

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


let rec string_of_mono_type = function
    | TVar t -> Int.to_string t
    | TPrim PInt -> "Int"
    | TPrim PBool -> "Bool"
    | TFun (m, n) -> "(" ^ (string_of_mono_type m) ^ " -> " ^ (string_of_mono_type n) ^ ")"
    | TProd l -> 
            l
            |> join_with_commas string_of_mono_type
            |> fun csv -> "("^csv^")"

let rec decode_arg i =
    if i < 26 then (i+97) |> Char.of_int_exn |> String.of_char else (122 |> Char.of_int_exn |> String.of_char)^(decode_arg (i - 25))

let rec encode_arg s =
    String.fold s ~init:0 ~f:(fun acc c -> acc + ((Char.to_int c) - 97))


let rec string_of_expr = function
    | Lit (Int i) -> Int.to_string i
    | Lit (Bool b) -> Bool.to_string b
    | App (l, r) -> "(" ^ (string_of_expr l) ^ " " ^ (string_of_expr r) ^ ")"
    | Func (a, e) -> "(\\" ^ (decode_arg a) ^ " -> " ^ (string_of_expr e) ^ ")"
    | Arg a -> decode_arg a
    | Op o -> string_of_op o
    | Let (a, e, b) -> "let " ^ (decode_arg a) ^ " = " ^ (string_of_expr e) ^ " in " ^ (string_of_expr b)
    | LetRec (a, e, b) -> "let rec " ^ (decode_arg a) ^ " = " ^ (string_of_expr e) ^ " in " ^ (string_of_expr b)
    | Tuple t -> join_with_commas string_of_expr t

let rec string_of_poly_type = 
    function
    | Mono m -> "forall [] "^(string_of_mono_type m)
    | Poly (tvns, m) -> "forall ["^(Set.to_list tvns |> join_with_commas decode_arg )^"] "^(string_of_mono_type m)
    
