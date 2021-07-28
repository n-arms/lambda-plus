open Parse
open Base
open Stdio
open Infer
open Ast

let run_typing_test expr =
    print_endline (string_of_expr expr);
    let open Option in
    match expr 
    |> infer (Map.empty (module Int)) with
    | Ok (s, t) -> print_endline (Ast.string_of_mono_type (Sub.apply_mono s t))
    | Error e -> print_endline ("Error: "^e)

let repl text = 
    let result = 
        text
        |> Lex.lex
        |> make
        |> parse_expr in
    match result with
    | Ok out, _ -> 
            run_typing_test out
    | _ -> print_endline "failed to parse text"

let rec main _ =
    let open Option in
    (In_channel.input_line stdin)
    >>| (fun line -> repl line)
    |> value ~default:();
    main 0



let _ = main 0
