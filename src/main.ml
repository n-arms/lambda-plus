open Stdio
open Parse
open Base
open Expr
open Typed_expr

let print_parse_result show (out, state) =
    ("with state "^(to_string state)^" ")
    |> (^) (match out with
    | Ok o -> "parsed to ["^(show o)^"] "
    | Error e -> "ERROR! ["^(from_parse_error e)^"] ")
    |> print_endline

let result = parse_expr (make "(\\a -> 3)")
let () = print_parse_result string_of_expr result
let () = 
    match result with
    | (Ok out, _) -> 
        (get_type (out) (Base.Map.Poly.empty))
        |> Result.map_error ~f:(fun e ->
                (string_of_typing_error e))
        |> Result.ok_or_failwith
        |> string_of_expr_type
        |> print_endline
    | (Error e, _) -> ()
