open Base

type t = char list

type expr =
    | Num of int
    | App of expr * expr
    | Func of string * expr
    | Arg of string

type parseStatus = {rest: t; pos: int}
type parseError = string

let explode s =
    let out = ref [] in
    for i = (let open String in length s) - 1 downto 0 do
        out := s.[i]::!out done;
        !out

let rec to_string = function
    | hd::tl -> (let open Char in to_string hd)^(to_string tl)
    | [] -> ""

