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
    " with leftover "^
    (List.fold rest ~init:"" ~f:(fun s c -> s^(Lex.to_string c)))

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
    | Ok _, state -> Ok (Lit (Bool true)), state
    | Error _, _ -> (match (token (Lex.Bool false)) state with
    | Ok _, state -> Ok (Lit (Bool false)), state
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

let rec parse_func state =
    ((token (Lex.Op Lex.Lambda))
    *> parse_arg
    <* (token (Lex.Op Lex.Arrow))
    >>= fun a ->
        parse_expr >>| fun e -> Func (a, e)) state
and parse_non_let state =
    match parse_func state with
    | Ok f, state -> Ok f, state
    | _ -> (match parse_arg state with
    | Ok a, state -> Ok (Arg a), state
    | _ -> (match parse_paren state with
    | Ok p, state -> Ok p, state
    | _ -> (match parse_num state with
    | Ok n, state -> Ok (Lit(Int n)), state
    | _ -> (match parse_bool state with
    | Ok b, state -> Ok b, state
    | _ -> (match parse_op state with
    | Ok o, state -> Ok (Op o), state
    | Error e, _ -> Error e, state)))))
and parse_non_app state =
    match parse_let state with
    | (Ok l, state) -> (Ok l, state)
    | _ -> (match parse_non_let state with
    | (Ok n, state) -> (Ok n, state)
    | (Error e, _) -> (Error e, state))
and parse_expr state =
    let terms = ref [] in
    let term = ref (parse_non_app state) in
    while (
        match !term with
        |(Ok _, _) -> true
        | _ -> false
    ) do
        let (current, state) = !term in
        terms := current::!terms;
        term := parse_non_app state
    done;
    let open List in
    let open Result in
    let (_, new_state) = !term in
    match rev !terms with
    | hd::h2::tl -> (Ok (
        fold_left (h2::tl) ~init:(ok_or_failwith hd) ~f:(fun acc x ->
            App (acc, ok_or_failwith x))), new_state)
    | [hd] -> (hd, new_state)
    | [] -> (Error "failed to parse expr", state)
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
(*and parse_tuple (state : t) : (expr, parseError) Result.t * t = *)


