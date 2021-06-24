open Base

type typing_error = string

type expr_type =
    | TEInt
    | TEGeneric
    | TEFunc of expr_type * expr_type

type typed_expr =
    | Num of int
    | App of expr_type * Expr.expr * Expr.expr
    | Func of expr_type * expr_type * string * Expr.expr
    | Arg of expr_type * string
    | Op of Expr.op

let string_of_typing_error e = e

let rec string_of_expr_type = function
    | TEInt -> "Num"
    | TEGeneric -> "<gen>"
    | TEFunc (t1, t2) -> 
            "("^
            (string_of_expr_type t1)^
            " -> "^
            (string_of_expr_type t2)^
            ")"

let string_of_typed_expr = function
    | Num n -> 
            "("^
            (let open Int in to_string n)^
            "::Num)"
    | App (r, e1, e2) -> 
            "(("^
            (let open Expr in string_of_expr e1)^
            " "^
            (let open Expr in string_of_expr e2)^
            ")::"^
            (string_of_expr_type r)^
            ")"
    | Func (t1, t2, s, e2) ->
            "(\\"^
            s^
            "::"^
            (string_of_expr_type t1)^
            " -> "^
            (let open Expr in string_of_expr e2)^
            "::"^
            (string_of_expr_type t2)^
            ")"
    | Arg (t, s) ->
            "("^
            s^
            "::"^
            (string_of_expr_type t)^
            ")"
    | Op o -> (let open Expr in string_of_op o)

let rec get_type expr (scope : (string, expr_type, 'a) Base.Map.t) =
    match expr with
    | Expr.Num _ -> Ok TEInt
    | Expr.App (e1, e2) -> Ok TEInt
    | Expr.Func (s, e) -> 
            let open Result in
            (get_type e (Map.set scope s TEGeneric))
            >>= fun t ->
                Ok (TEFunc (TEGeneric, t))
    | Expr.Arg s -> 
            Result.of_option (Map.find scope s) ~error:("variable "^s^"not in scope")
    | Expr.Op _ -> Ok TEInt
