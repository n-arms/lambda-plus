open Typed_expr
open Type_infer
open Parse
open Stdio

let result = parse_expr (make "(\\a -> \\b -> + a 1)")
let () = 
    match result with
    | Ok out, _ -> (
            match infer_type out with 
            | Ok t -> print_endline (string_of_expr_type t)
            | Error e -> print_endline (Type_unify.string_of_typing_error e))
    | _ -> print_endline "parse failed"

