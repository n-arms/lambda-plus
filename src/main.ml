open Stdio
open Parse
open Base

let print_parse_result show (out, state) =
    ("with state "^(to_string state)^" ")
    |> (^) (match out with
    | Ok o -> "parsed to ["^(show o)^"] "
    | Error e -> "ERROR! ["^(from_parse_error e)^"] ")
    |> print_endline

let result = parse_arg (make "123 argB")
let () = print_parse_result (fun x -> from_expr (Arg x)) result

let result = parse_num (make "123 arg")
let () = print_parse_result (fun x -> from_expr (Num x)) result

let result = parse_expr (make "\\a -> b")
let () = print_parse_result from_expr result
