open Stdio
open Parse
open Base

let print_parse_result show (out, state) =
    ("with state "^(to_string state)^" ")
    |> (^) (match out with
    | Ok o -> "parsed to ["^(show o)^"] "
    | Error e -> "ERROR! ["^(from_parse_error e)^"] ")
    |> print_endline

let result = parse_expr (make "(\\y -> (\\f -> f (x x)) \\f -> f (x x)) fib 42" )
let () = print_parse_result from_expr result
