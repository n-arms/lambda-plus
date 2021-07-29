open Parse
open Base
open Stdio
open Infer
open Ast
open Interpreter
let run_typing_test expr =
    print_endline (string_of_expr expr);
    let open Option in
    match expr 
    |> infer (Map.empty (module Int)) with
    | Ok (s, t) -> print_endline (Ast.string_of_mono_type (Sub.apply_mono s t))
    | Error e -> print_endline ("Error: "^e)


let repl text = 
    let (e, _) =
    text
    |> Lex.lex
    |> make
    |> parse_expr in
    e
    |> Result.map_error ~f:Parse.from_parse_error
    |> Result.bind ~f:(fun e -> Result.map (infer (Map.empty (module Int)) e) ~f:(fun r -> (e, r)))
    |> Result.map ~f:(fun (e, (_, t)) -> (e, ":: "^(Ast.string_of_mono_type t)))
    |> fun r -> (
        match r with
        | Ok (e, s) -> print_endline s; Some e
        | Error e -> print_endline e; None
    )
    |> Option.map ~f:(eval (Map.empty (module Int)))
    |> Option.map ~f:Ast.string_of_expr
    |> Option.map ~f:print_endline

let rec main _ =
    let open Option in
    (In_channel.input_line stdin)
    >>= (fun line -> repl line)
    |> value ~default:();
    main 0



let _ = main 0

(*
let _ = 
    let open Option in
    match (In_channel.input_line stdin) with
    | Some text ->
            text
            |> Lex.lex
            |> make
            |> Parse.parse_expr
            (*|> Parse.parse_non_app*)
            |> fun (out, state) -> 
                    out
                    |> Result.map ~f:Ast.string_of_expr
                    |> Result.map_error ~f:Parse.from_parse_error
                    |> fun o -> (match o with | Ok s -> print_endline s | Error e -> print_endline e);
                    state
                    |> Parse.to_string
                    |> print_endline
    | None -> ()*)
