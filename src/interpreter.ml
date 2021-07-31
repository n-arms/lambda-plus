open Base
open Ast

exception UncaughtTypeError of string

let debug _msg value =
    (*uncomment this \/ line for some debug info during evaluation *)
    (*Stdio.print_endline (_msg ^ " " ^ (string_of_expr value));*) 
    value

let string_of_env =
    Map.fold ~init:"" ~f:(fun ~key:x ~data:y acc ->
        acc^" "^(decode_arg x)^" => "^(string_of_expr y))
    
let rec bind_pattern env (pat : pattern) (expr : expr) =
    print_endline ("calling bind pattern with pattern "^(string_of_pattern pat)^" and env "^(string_of_env env)^" and expr "^(string_of_expr expr));
    match pat with
    | AllOf (VarPat v) -> Map.set env ~key:v ~data:expr
    | AllOf (LitPat l) -> 
            if Poly.equal (Lit l) expr then
                env
            else
                raise (UncaughtTypeError "failed literal match")
    | AllOf (ProdPat p) -> 
            (match expr with
            | Tuple l -> 
                    List.zip_exn p l
                    |> List.map ~f:(fun (pat, expr) -> bind_pattern env (AllOf pat) expr)
                    |> List.fold ~init:(Map.empty (module Int)) ~f:(fun acc x ->
                            Map.merge_skewed x acc ~combine:(fun ~key:_ v _ -> v))
                    |> Map.merge env ~f:(fun ~key:_ pat ->
                            match pat with
                            | `Both (_, r) -> Some r
                            | `Left l -> Some l
                            | `Right r -> Some r)
            | _ -> raise (UncaughtTypeError "cannot patch product pattern without tuple"))
    | OneOf(_) -> env

let rec free_in_pattern = function
    | AllOf (VarPat v) -> [v]
    | AllOf (ProdPat p) -> List.fold p ~init:[] ~f:(fun acc x -> acc @ (free_in_pattern (AllOf x)))
    | _ -> []


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
    print_endline ("calling eval with env "^(string_of_env env)^" and expr "^(string_of_expr expr));
    match expr with
    | Lit (Int _) -> expr |> debug "evaluated num cell"
    | Lit (Bool _) -> expr
    | Op _ -> expr
    | App (e1, e2) ->
            (match eval env e1 with
            | Func(x, e') ->
                (*eval (Map.set env ~key:0 ~data:e2) e' |> debug "evaluated App (Func, _) cell (if this isnt working its cause i havent done pattern matching yet ok, chill)"*)
                    eval (bind_pattern env x e2 |> fun x -> print_endline ("binded pattern to env "^(string_of_env x)); x) e'

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
            eval env (Let (x, App (Op Fix, Func (AllOf (VarPat x), e1)), e2)) |> debug "evaluated a let rec cell"
    | Tuple l ->
            Tuple (List.map l ~f:(eval env)) |> debug "evaluated a tuple cell "
    | Func (x, e) -> Func (x, eval (List.fold (free_in_pattern x) ~init:env ~f:(fun acc x -> Map.remove acc x)) e) |> debug "evaluated a cell"
