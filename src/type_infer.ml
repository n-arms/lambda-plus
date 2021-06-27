open Base
open Expr
open Typed_expr

type scope = (string, expr_type, Base.String.comparator_witness) Base.Map.t
let empty = Base.Map.empty (module String)

type typing_error = string
let string_of_typing_error x = x
(*
let rec infer_type e s =
    match e with
    | Num _ -> Ok (TEInt, s)
    | App (e1, e2) -> 
            (match infer_type e1 s with (*remove arg from scope after *)
            | Ok (TEFunc (t1, t2), _) -> 
                    (match infer_type e2 s with
                    | 
                    let open Base.Poly in
                    if t1 = e2t then Ok (t2, s) else Error "Function types do not match"
            | _ -> Error "Cannot apply non-function")
    | Func (a, e) -> 
            match infer_type e (let open Map in set s a (next_generic ())) with
            | Ok (t, s2) -> Ok (TEFunc (let open Map in find_exn s2 a, t))
            | _ -> Error "failed to construct function type"
    | Arg a -> 
            match let open Map in find s a with
            | Some t -> Ok (t, s)
            | None -> Error "arg not in scope"
    | Op o -> TEFunc (TEInt, TEFunc (TEInt, TEInt))
*)

let id = ref 0
let next_generic () = id := !id + 1; TEGeneric !id

let rec infer_type e (s : (string, expr_type, 'a) Map.t) =
    let open Map in
    let open Result in
    match e with
    | Arg a -> 
            (of_option (find s a) ~error:"arg not in scope")
            >>| (fun t -> t, s)
    | Num _ -> Ok (TEInt, s)
    | Op o -> Ok (TEFunc (TEInt, TEFunc (TEInt, TEInt)), s)
    | Func (a, e) -> 
            (infer_type e (set s ~key:a ~data:(next_generic ())))
            >>| (fun (t, s2) -> TEFunc (find_exn s2 a, t), s)
    | App (l, r) -> 
            (Error "not implemented")
