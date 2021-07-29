open Base
open Ast

exception UncaughtTypeError of string

let debug _msg value =
    (*uncomment this \/ line for some debug info during evaluation *)
    (*Stdio.print_endline (_msg ^ " " ^ (string_of_expr value));*) 
    value

let rec eval (env : expr Map.M(Int).t) expr =
    let eval_op env o e1 e2 =
        match (eval env e1, eval env e2) with
        | Lit (Int m), Lit (Int n) -> (
                match o with
                | Add -> Lit (Int (m + n))
                | Sub -> Lit (Int (m - n))
                | Mul -> Lit (Int (m * n))
                | Div -> Lit (Int (m / n))
                | Mod -> Lit (Int (m % n) )
                | _ -> raise (UncaughtTypeError "attempt to eval op on fix or unary minus"))
        | _, _ -> raise (UncaughtTypeError "attempt to eval op with a non-number value") in
    match expr with
    | Lit (Int _) -> expr |> debug "evaluated num cell"
    | App (e1, e2) ->
            (match eval env e1 with
            | Func(x, e') ->
                eval (Map.set env ~key:x ~data:e2) e' |> debug "evaluated App (Func, _) cell"
            | Op Fix -> 
                    eval env (App (e2, App(Op Fix, e2))) |> debug "evaluated App (Fix, _) cell"
            | Op UnaryMinus -> 
                    (match eval env e2 with
                    | Lit (Int n) -> Lit (Int (-1 * n)) |> debug "evaluated App (~, _) cell"
                    | _ -> raise (UncaughtTypeError "attempt to apply operator \"~\" to a value that is not a number"))
            | App (Op o, e3) -> eval_op env o e3 e2 |> debug "evaluated App(App(op, _), _) cell"
            | x -> App (x, e2)) |> debug "returned input of a failed app cell"
    | Arg a -> Map.find_exn env a |> eval env |> debug "evaluated an arg cell"
    | Let (x, e1, e2) ->  
            eval (Map.set env ~key:x ~data:e1) e2 |> debug "evaluated a let cell"
    | LetRec (x, e1, e2) ->
            eval env (Let (x, App (Op Fix, Func (x, e1)), e2)) |> debug "evaluated a let rec cell"
    | Tuple l ->
            Tuple (List.map l ~f:(eval env)) |> debug "evaluated a tuple cell "
    | x -> x |> debug "evaluated a cell"
