open Base

type t = {rest: char list; pos: int}
type parseError = string
type 'a parser = t -> ('a, parseError) result*t

type expr =
    | Num of num
    | App of app
    | Func of func
    | Arg of arg
    | Op of op
and num = int
and app = expr * expr
and func = string * expr
and arg = string
and op =
    | UnaryMinus
    | Sub
    | Add
    | Div
    | Mul
    | Mod

let from_string s =
    let out = ref [] in
    for i = (let open String in length s) - 1 downto 0 do
        out := s.[i]::!out done;
        !out

let from_char_list l = 
    let open List in
    fold l ~init:"" ~f:(fun acc x ->
        acc^(let open Char in to_string x))

let from_parse_error p = p

let from_op = function
    | UnaryMinus -> "~"
    | Sub -> "-"
    | Add -> "+"
    | Div -> "/"
    | Mul -> "*"
    | Mod -> "%"

let rec from_expr = function
    | Num i -> (let open Int in to_string i)
    | App (e1, e2) -> "("^(from_expr e1)^" "^(from_expr e2)^")"
    | Func (a, e) -> "(\\"^a^" -> "^(from_expr e)^")"
    | Arg a -> a
    | Op o -> (from_op o)

let make s = {rest = from_string s; pos = 1}

let to_string {rest; pos} =
    "parsed to pos "^
    (let open Int in to_string pos)^
    " with leftover "^
    (from_char_list rest)

let chars c {rest; pos} =
    let open Base.Poly in
    let rec is_match a b =
        match (a, b) with
        | (hd1::tl1, hd2::tl2) when hd1 = hd2 -> is_match tl1 tl2
        | ([], l) -> Some l
        | _ -> None
    in
    let open List in
    match is_match c rest with
    | Some l -> (Ok c, {rest = l; pos = pos + (length c)})
    | None -> (Error "chars failed to match", {rest;pos})

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
    let open Char in
    (many1 (select is_alpha)) >>| from_char_list

let parse_num =
    let open Char in
    let open Int in
    (many1 (select is_digit)) >>| (fun c ->
        c |> from_char_list |> of_string)

let parse_op {rest;pos} =
    match rest with
    | '~'::tl -> (Ok UnaryMinus, {rest = tl; pos = pos + 1})
    | '-'::tl -> (Ok Sub, {rest = tl; pos = pos + 1})
    | '+'::tl -> (Ok Add, {rest = tl; pos = pos + 1})
    | '*'::tl -> (Ok Mul, {rest = tl; pos = pos + 1})
    | '/'::tl -> (Ok Div, {rest = tl; pos = pos + 1})
    | '%'::tl -> (Ok Mod, {rest = tl; pos = pos + 1})
    | _ -> (Error "failed to parse operator", {rest;pos})

let rec parse_func state =
    (let open Char in
    (chars ('\\'::[]))
    *> parse_arg
    <* (many (select is_whitespace))
    <* (chars ('-'::'>'::[]))
    <* (many (select is_whitespace))
    >>= fun a ->
        parse_expr >>| fun e -> (a, e)) state
and parse_non_app state =
    match parse_func state with
    | (Ok f, state) -> (Ok (Func f), state)
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
        |(Ok v, _) -> true
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
