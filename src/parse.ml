open Base
open Expr

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
    >>| (function Lex.Arg a -> a | _ -> "")

let parse_num =
    select (function Lex.Num _ -> true | _ -> false)
    >>| (function Lex.Num n -> (Int.of_string n) | _ -> 0)

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
        | _ -> (Error "did not find op token"), old_state)
    | _ -> (Error "did not find op token", {rest;pos})

let rec parse_func state =
    ((token (Lex.Op Lex.Lambda))
    *> parse_arg
    <* (token (Lex.Op Lex.Arrow))
    >>= fun a ->
        parse_expr >>| fun e -> Func (a, e)) state
and parse_non_app state =
    match parse_func state with
    | (Ok f, state) -> (Ok f, state)
    | _ -> (match parse_arg state with
    | (Ok a, state) -> (Ok (Arg a), state)
    | _ -> (match parse_paren state with
    | (Ok p, state) -> (Ok p, state)
    | _ -> (match parse_num state with
    | (Ok n, state) -> (Ok (Num n), state)
    | _ -> (match parse_op state with
    | (Ok o, state) -> (Ok (Op o), state)
    | _ -> (Error "failed to parse non-app", state)))))
and parse_expr state =
    let open Char in 
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
and parse_let state =
    (*
    ((chars ('l'::'e'::'t'::[]))
    *> parse_arg
    <* (chars ['='])
    >>| fun s -> (
        parse_expr
        <* (chars ('i'::'n'::[]))
        >>| fun e ->
            parse_expr
            >>| fun e2 -> Ok (Let (s, e, e2)))) state*)
    ((token (Lex.Op Lex.Let))
    *> parse_arg
    <* (token (Lex.Op Lex.Equals))
    >>| fun s -> (
        parse_expr
        <* (token (Lex.Op Lex.In))
        >>| fun e ->
            parse_expr
            >>| fun e2 -> Ok (Let (s, e, e2)))) state
(*
let rec parse_func state =
    (let open Char in
    (chars ('\\'::[]))
    *> parse_arg
    <* (many (select is_whitespace))
    <* (chars ('-'::'>'::[]))
    <* (many (select is_whitespace))
    >>= fun a ->
        parse_expr >>| fun e -> Func (a, e)) state
and parse_non_let state =
    match parse_func state with
    | (Ok f, state) -> (Ok f, state)
    | _ -> (match parse_arg state with
    | (Ok a, state) -> (Ok (Arg a), state)
    | _ -> (match parse_paren state with
    | (Ok p, state) -> (Ok p, state)
    | _ -> (match parse_num state with
    | (Ok n, state) -> (Ok (Num n), state)
    | _ -> (match parse_op state with
    | (Ok o, state) -> (Ok (Op o), state)
    | _ -> (Error "failed to parse non-app", state)))))
and parse_non_app state =
    match parse_let state with
    | (Ok l, state) -> (Ok l, state)
    | _ -> (match parse_non_let state with
    | (Ok e, state) -> (Ok e, state)
    | (Error e, _) -> (Error e, state))
and parse_expr state =
    let open Char in 
    let terms = ref [] in
    let term = ref (parse_non_app state) in
    while (
        match !term with
        |(Ok _, _) -> true
        | _ -> false
    )do
        let (current, state) = !term in
        terms := current::!terms;
        term := ((many (select is_whitespace)) *> parse_non_app) state
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
    ((chars ('('::[]))
    *> parse_expr
    <* (chars (')'::[]))) state
and parse_let (state : t) : (expr, parseError) Result.t * t =
    let open Char in
    ((chars ('l'::'e'::'t'::[]))
    *> (many (select is_whitespace))
    *> parse_arg
    <* (many (select is_whitespace))
    <* (chars ['='])
    <* (many (select is_whitespace))
    >>= fun s -> 
        parse_expr
        <* (many (select is_whitespace))
        <* (chars ('i'::'n'::[]))
        <* (many (select is_whitespace))
        >>= fun e -> 
            parse_expr
            >>| fun e2 -> Let (s, e, e2)) state*)
