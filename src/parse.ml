open Base

type t = {rest: char list; pos: int}
type parseError = string

type expr =
    | Num of num
    | App of app
    | Func of func
    | Arg of arg
and num = int
and app = expr * expr
and func = string * expr
and arg = string

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

let rec from_expr = function
    | Num i -> (let open Int in to_string i)
    | App (e1, e2) -> "("^(from_expr e1)^" "^(from_expr e2)^")"
    | Func (a, e) -> "(\\"^a^" -> "^(from_expr e)^")"
    | Arg a -> a

let make s = {rest = from_string s; pos = 1}

let to_string {rest; pos} =
    "parsed to pos "^
    (let open Int in to_string pos)^
    " with leftover "^
    (from_char_list rest)

let rec chars c {rest; pos} = 
    let open Base.Poly in
    match (c, rest) with
    | (ch::ct, th::rest) when ch = th -> chars ct {rest; pos = pos + 1}
    | ([], _) -> (true, {rest; pos})
    | _ -> (false, {rest; pos})

let rec many p s =
    match p s with
    | (Ok r, s) -> 
            let open Result in
            let (l, s) = many p s in
            (l >>= (fun l -> Ok (r::l)), s)
    | _ -> (Ok [], s)

let many1 p s =
    match p s with
    | (Ok out, s) -> 
            let open Result in 
            let (l, s) = many p s in
            (l >>= (fun l -> Ok (out::l)), s)
    | (Error e, s) -> (Error e, s)

let select pred = function
    | {rest = hd::tl; pos} when pred hd -> (Ok hd, {rest = tl; pos = pos+1})
    | s -> (Error "predicate failed to match", s)

let parse_arg state = 
    let open Char in
    let (out, state) = many1 (select is_alpha) state in
    let open Result in
    (out >>= (fun c -> Ok (from_char_list c)), state)
