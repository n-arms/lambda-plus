open Expr
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

let rec apply s t =
    List.fold_right s ~f:(fun (x, e) -> subst e x) ~init:t

let rec unify_one s t =
    match (s, t) with
    | (TEGeneric x, TEGeneric y) -> let open String in if x = y then [] else [(x, t)]
    | (TEArrow (x, y), TEArrow (u, v)) -> unify [(x, u); (y, v)]
    | ((TEGeneric x, (TEArrow (u, v) as z)) | ((TEArrow (u, v) as z), TEGeneric x)) ->
            if occurs x z
            then failwith "not unifiable: infinite type"
            else [(x, z)]
    | (TEGeneric x, TEInt) | (TEInt, TEGeneric x) ->
            [(x, TEInt)]
    | (TEArrow _, TEInt) | (TEInt, TEArrow _) ->
            failwith "not unifiable: int and function"
and unify = function
    | [] -> []
    | (x, y) :: t -> 
            let t2: (string * expr_type) list = unify t in
            let t1: (substitution) = unify_one (apply t2 x) (apply t2 y) in
            t1 @ t2
