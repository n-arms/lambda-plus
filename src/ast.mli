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


type lit =
    | Bool of bool
    | Int of int

type exhaustive_pattern = 
    | LitPat of lit
    | VarPat of var_name
    | ProdPat of exhaustive_pattern list

type pattern =
    | AllOf of exhaustive_pattern
    | OneOf of pattern list

type expr =
    | Lit of lit
    | App of expr * expr
    | Func of pattern * expr
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

val string_of_op : op -> string
val string_of_expr : expr -> string
val string_of_mono_type : mono_type -> string
val string_of_poly_type : poly_type -> string
val decode_arg : var_name -> string
val encode_arg : string -> var_name
val string_of_exhaustive_pattern : exhaustive_pattern -> string
val string_of_pattern : pattern -> string
