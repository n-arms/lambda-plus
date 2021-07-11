open Parse
open Stdio
open Base
(*
let result = parse_expr (make "\\a -> \\b -> a b")
let () = 
    match result with
    | Ok out, _ -> (
            match infer_type out with 
            | Ok t -> print_endline (string_of_expr_type t)
            | Error e -> print_endline (Type_unify.string_of_typing_error e))
    | _ -> print_endline "parse failed"

let result = parse_expr (make "\\a -> b")
let () = 
    match result with
    | Ok out, _ -> 
            out
            |> Expr.string_of_expr
            |> print_endline
    | Error e, _ -> 
            e
            |> Parse.from_parse_error
            |> print_endline*)

let tokens = Lex.lex "\\a -> \\b -> * (+ a 3) b"
let () =
    List.fold tokens ~init:() ~f:(fun _ t -> print_endline (Lex.to_string t))
