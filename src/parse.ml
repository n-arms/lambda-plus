open Base
open Ast
open Util

type t = {rest: Lex.token list; pos: int}
type parseError = string
type 'a parser = t -> ('a, parseError) Result.t*t

let from_parse_error p = p


let make l = {rest = l; pos = 0}

let to_string {rest; pos} =
    "parsed to pos "^
    (let open Int in to_string pos)^
    " with leftover \""^
    (List.fold rest ~init:"" ~f:(fun s c -> s^(Lex.to_string c)))^
    "\""

let debug _msg value =
    (*Stdio.print_endline (_msg ^ " " ^ (to_string value)); value*)
    value

let (>>=) p f state = 
    match p state with
    | (Ok a, state) -> f a state
    | (Error e, _) -> (Error e, state)

let (>>|) p f state =
    match p state with
    | (Ok a, state) -> (Ok (f a), state)
    | (Error e, state) -> (Error e, state)

let ( *>) a b =
    a >>= (fun _ -> b)

let (<*) a b =
    a >>= (fun out -> b >>| (fun _ -> out))

let token t = function
    | {rest = hd::tl; pos} when let open Poly in (hd = t) -> (Ok hd, {rest=tl; pos=pos+1})
    | s -> Error ("failed to match token "^(Lex.to_string t)), s

let rec many p s =
    match p s with
    | (Ok r, s) -> 
            let open Result in
            let (l, s) = many p s in
            (l >>= (fun l -> Ok (r::l)), s)
    | _ -> (Ok [], s)

let many1 p =
    p >>= (fun out ->
        (many p) >>| (fun l -> out::l))

let (<|>) p q state =
    match p state with
    | (Ok _, _) as out -> out
    | Error _, _ -> q state


let select pred = function
    | {rest = hd::tl; pos} when pred hd -> (Ok hd, {rest = tl; pos = pos+1})
    | s -> (Error "predicate failed to match", s)

let parse_arg = 
    select (function Lex.Arg _ -> true | _ -> false)
    >>| (function Lex.Arg a -> Ast.encode_arg a | _ -> 0)

let parse_num =
    select (function Lex.Num _ -> true | _ -> false)
    >>| (function Lex.Num n -> (Int.of_string n) | _ -> 0)

let parse_bool state =
    match (token (Lex.Bool true)) state with
    | Ok _, state -> Ok true, state
    | Error _, _ -> (match (token (Lex.Bool false)) state with
    | Ok _, state -> Ok false, state
    | Error e, _ -> Error e, state)

let parse_op {rest;pos} =
    match rest with
    | Lex.Op o::tl -> (
        let old_state = {rest;pos} in
        let new_state = {rest=tl;pos=pos+1} in
        match o with
        | Lex.Times -> (Ok Mul), new_state
        | Lex.Plus -> (Ok Add), new_state
        | Lex.Minus -> (Ok Sub), new_state
        | Lex.UnaryMinus -> (Ok UnaryMinus), new_state
        | Lex.Slash -> (Ok Div), new_state
        | Lex.Fix -> (Ok Fix), new_state
        | _ -> (Error "did not find op token"), old_state)
    | _ -> (Error "did not find op token", {rest;pos})

let rec parse_exhaustive_pattern (state : t) : (exhaustive_pattern, parseError) Result.t * t = 
    ((parse_arg >>| fun a -> VarPat a)
    <|> (parse_num >>| fun n -> LitPat (Int n))
    <|> (parse_bool >>| fun b -> LitPat (Bool b))
    <|> (
        token Lex.LPar
        *> parse_exhaustive_pattern
        >>= fun start ->
            many (
                token (Lex.Op Lex.Comma)
                *> parse_exhaustive_pattern
            )
            <* token Lex.RPar
            >>| fun terms ->
                if List.length terms = 0 then
                    start
                else
                    ProdPat (start::terms))) state

let parse_pattern = 
    parse_exhaustive_pattern (* XXX add sum types so this becomes more interesting *)
    >>| fun ep -> AllOf ep

(*let rec parse_func state =
    ((token (Lex.Op Lex.Lambda))
    *> parse_arg
    <* (token (Lex.Op Lex.Arrow))
    >>= fun a ->
        parse_expr >>| fun e -> Func (a, e)) state*)
let rec parse_func state =
    ((token (Lex.Op Lex.Lambda))
    *> parse_pattern
    <* (token (Lex.Op Lex.Arrow))
    >>= fun a ->
        parse_expr >>| fun e -> Func (a, e)) state
and parse_non_let state =
    match parse_func state with
    | Ok f, state -> Ok f, state
    | _ -> (match parse_arg state with
    | Ok a, state -> Ok (Arg a), state
    | _ -> (match parse_paren state with | Ok p, state -> Ok p, state | _ -> (match parse_num state with
    | Ok n, state -> Ok (Lit(Int n)), state
    | _ -> (match parse_bool state with
    | Ok b, state -> Ok (Lit (Bool b)), state
    | _ -> (match parse_op state with
    | Ok o, state -> Ok (Op o), state
    | Error e, _ -> Error e, state)))))
and parse_non_app state =
    match parse_tuple (state |> debug "parsing maybe tuple: ") with
    | (Ok l, state) -> (Ok l, state)
    | _ -> (match parse_non_tuple state with
    | (Ok n, state) -> (Ok n, state)
    | (Error e, _) -> (Error e, state))
and parse_expr state =
    let open Option in
    let new_state = ref None in
    unfold ~init:state ~f:(fun state ->
        new_state := Some state;
        let current, state = parse_non_app state in
        Result.ok current
        >>| fun current ->
            (current, state))
    |> List.reduce ~f:(fun a b -> App (a, b))
    >>= (fun l ->
        !new_state
        >>| fun s -> (l, s))
    |> fun r -> (
        match r with
        | Some (l, s) -> Ok l, s
        | _ -> Error "could not parse at least 1 non-app for application", state)
and parse_paren state =
    ((token Lex.LPar)
    *> parse_expr
    <* (token Lex.RPar)) state
and parse_let (state : t) : (expr, parseError) Result.t*t =
    ((fun state -> (match ((token (Lex.Op Lex.Let)) <* (token (Lex.Op Lex.Rec))) state with
    | Ok _, state -> Ok ((fun x e1 e2 -> LetRec (x, e1, e2))), state
    | Error _, _ -> (((token (Lex.Op Lex.Let)) >>| (fun _ x e1 e2 -> Let(x, e1, e2))) state)))
    >>= fun f -> ((parse_arg
    <* (token (Lex.Op Lex.Equals))
    >>= fun s -> (
        parse_expr
        <* (token (Lex.Op Lex.In))
        >>= fun e -> (
            parse_expr
            >>| fun e2 -> f s e e2))))) state
and parse_non_tuple (state : t) = 
    match parse_let state with
    | (Ok l, state) -> (Ok l, state)
    | _ -> (match parse_non_let state with
    | (Ok n, state) -> (Ok n, state)
    | (Error e, _) -> (Error e, state))
and parse_tuple (state : t) : (expr, parseError) Result.t * t = 
    let open Option in
    match parse_non_tuple state with 
    | Ok start, new_state -> (
        let new_state = ref new_state in
        unfold ~init:!new_state ~f:(fun state ->
            new_state := state;
            let (current, state) = ((token (Lex.Op Lex.Comma))
            *> parse_non_tuple) state in 
            Result.ok current
            >>| fun current -> (current, state))
        |> fun l ->
                if List.length l >= 1 then
                    Ok (Tuple (start::l)), !new_state
                else
                    Error "could not parse at least 2 terms for a tuple", state
    )
    | Error e, _ -> Error e, state

