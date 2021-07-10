open Typed_expr
open Type_infer
open Parse
open Stdio

let result = parse_expr (make "\\x -> x x")
let () = 
    match result with
    | Ok out, _ -> 
            print_endline (string_of_expr_type (infer_type out))
    | _ -> print_endline "parse failed"

