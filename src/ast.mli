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

type expr =
    | Num of int
    | App of expr * expr
    | Func of var_name * expr
    | Arg of var_name
    | Op of op
    | Let of var_name * expr * expr

type mono_type =
    | TVar of type_var_name
    | TInt
    | TFun of (mono_type * mono_type)

type tvset = Set.M(Int).t

type poly_type =
    | Mono of mono_type
    | Poly of (tvset * mono_type)

val string_of_op : op -> string
val string_of_expr : expr -> string
val string_of_mono_type : mono_type -> string
val decode_arg : var_name -> string
val encode_arg : string -> var_name
