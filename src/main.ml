open Parse
open Stdio
open Base
let result = parse_expr (make (Lex.lex "\\a -> \\b -> (\\c -> b) a" ))
(*
let () = 
    match result with
    | Ok out, _ -> (
            match infer_type out with 
            | Ok t -> print_endline (string_of_expr_type t)
            | Error e -> print_endline (Type_unify.string_of_typing_error e))
    | _ -> print_endline "parse failed"
*)
let () = 
    match result with
    | Ok out, _ -> 
            out
            |> Expr.string_of_expr
            |> print_endline
    | Error e, _ -> 
            e
            |> Parse.from_parse_error
            |> print_endline
