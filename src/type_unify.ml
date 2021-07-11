open Typed_expr
open Base

type substitution = (string * expr_type) list

let rec occurs (x : string) (t : expr_type) =
    match t with
    | TEInt -> false
    | TEGeneric n -> let open String in x = n
    | TEArrow (u, v) -> occurs x u || occurs x v

(* declare that var x has type s in expression t *)
let rec subst s x t =
    match t with
    | TEInt -> t
    | TEGeneric y -> if (String.equal x y) then s else t 
    | TEArrow (u, v) -> TEArrow (subst s x u, subst s x v)

let apply s t =
    List.fold_right s ~f:(fun (x, e) -> subst e x) ~init:t

type typing_error = string
let string_of_typing_error e = e

let rec unify_one s t =
    match (s, t) with
    | (TEGeneric x, TEGeneric y) -> Ok (let open String in if x = y then [] else [(x, t)])
    | (TEArrow (x, y), TEArrow (u, v)) -> unify [(x, u); (y, v)]
    | ((TEGeneric x, (TEArrow _ as z)) | ((TEArrow _ as z), TEGeneric x)) ->
            if occurs x z
            then Error "not unifiable: infinite type"
            else Ok [(x, z)]
    | (TEGeneric x, TEInt) | (TEInt, TEGeneric x) ->
            Ok [(x, TEInt)]
    | (TEArrow _, TEInt) | (TEInt, TEArrow _) ->
            Error "not unifiable: int and function"
    | (TEInt, TEInt) -> Ok []

and unify = function
    | [] -> (Ok [])
    | (x, y) :: t -> 
            let open Result in
            (unify t)
            >>= fun t2 ->
                (unify_one (apply t2 x) (apply t2 y))
                >>= fun t1 -> Ok (t1 @ t2)
