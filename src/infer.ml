open Base
open Ast
open Sub

let generalize env m =
    let rec fv = function
        | Mono t ->
                (match t with
                | TVar n -> Set.singleton (module Int) n
                | TFun (m1, m2) ->
                        Set.union (fv (Mono m1)) (fv (Mono m2))
                | TPrim PInt -> Set.empty (module Int)
                | TProd l ->
                        List.fold l ~init:(Set.empty (module Int)) ~f:(fun acc x ->
                            Set.union acc (fv (Mono x)))
                | TPrim PBool -> Set.empty (module Int))
        | Poly (tvns, m) -> Set.diff (fv (Mono m)) tvns in
    let fvenv env = Map.fold env ~init:(Set.empty (module Int)) ~f:(fun ~key:_ ~data:poly fvset -> Set.union fvset (fv poly)) in
    Poly (Set.diff (fv (Mono m)) (fvenv env), m)

let code = ref 0
let newvar () = code := !code + 1; !code - 1

let inst = function
    | Poly (tvns, m) ->
            let sub = Set.fold tvns ~init:[] ~f:(fun sub tvn -> (tvn, TVar (newvar ()))::sub) in
            apply_mono sub m
    | Mono m -> m

let rec occurs tvn = function
    | TVar n when n = tvn -> true
    | TFun (m1, m2) when occurs tvn m1 || occurs tvn m2 -> true
    | _ -> false


let rec unify m1 m2 =
    match m1, m2 with
    | TPrim p, TPrim q when let open Base.Poly in p = q -> Ok []
    | TVar n1, TVar n2 when n1 = n2 -> Ok []
    | TVar n1, _ -> 
            if occurs n1 m2 then 
                Error (
                    "cannot unify types "^
                    (Ast.string_of_mono_type m1)^
                    " and "^
                    (Ast.string_of_mono_type m2)^
                    " because argument \""^
                    (Ast.decode_arg n1)^
                    "\" would cause an infinite type")
            else
                Ok [n1, m2]
    | _, TVar n2 ->
            if occurs n2 m1 then
                Error (
                    "cannot unify types "^
                    (Ast.string_of_mono_type m1)^
                    " and "^
                    (Ast.string_of_mono_type m2)^
                    " because argument \""^
                    (Ast.decode_arg n2)^
                    "\" would cause an infinite type")
            else
                Ok [n2, m1]
    | TFun (m11, m12), TFun (m21, m22) ->
            let open Result in
            (unify m11 m21)
            >>= fun s1 ->
                let s = apply_mono s1 in
                (unify (s m12) (s m22))
                >>| fun s2 ->  s2 @ s1
    | TProd l1, TProd l2 when List.length l1 = List.length l2 ->
            let open Result in
            List.zip_exn l1 l2
            |> List.fold ~init:(Ok []) ~f:(fun acc (x, y) ->
                    acc
                    >>= fun s1 ->
                        (unify (apply_mono s1 x) (apply_mono s1 y))
                        >>| fun s2 ->
                            s2 @ s1)

    | _ -> Error ("failed to unify types "^(Ast.string_of_mono_type m1)^" and "^(Ast.string_of_mono_type m2))

let rec infer (env : env) : expr -> (sub * mono_type, string) Result.t = function
    | Arg x -> 
            let open Result in
            (Map.find env x)
            |> of_option ~error:("failed to find argument "^(Ast.decode_arg x)^" in scope "^(Sub.string_of_env env))
            >>| fun t -> ([], inst t)
    | Lit (Int _) -> Ok ([], TPrim (PInt))
    | Lit (Bool _) -> Ok ([], TPrim (PBool))
    | Func (x, e1) -> 
            let open Result in
            let t = TVar (newvar ()) in
            (infer (Map.set env ~key:x ~data:(Mono t)) e1)
            >>| fun (s, t') -> (s, TFun (apply_mono s t, t'))
    | Let (x, e1, e2) -> 
            let open Result in
            (infer env e1)
            >>= fun (s0, t) ->
                (infer (Map.set env ~key:x ~data:(generalize (apply_over_env s0 env) t)) e2)
                >>| fun (s1, t') ->
                    (s1 @ s0, t')
    | LetRec (x, e1, e2) ->
            infer env (Let (x, App (Op Fix, Func (x, e1)), e2))
    | App (e1, e2) -> 
            let open Result in
            (infer env e1)
            >>= fun (s0, t0) ->
                (infer (apply_over_env s0 env) e2)
                >>= fun (s1, t1) ->
                    let t' = TVar (newvar ()) in
                    (unify (apply_mono s1 t0) (TFun (t1, t')))
                    >>| fun s2 ->
                        (s2 @ (s1 @ s0), apply_mono s2 t')
    | Op UnaryMinus -> Ok ([], TFun (TPrim PInt, TPrim PInt))
    | Op Fix -> 
            let t = TVar (newvar ()) in
            Ok ([], TFun (TFun (t, t), t))
    | Op _ -> Ok ([], TFun (TPrim PInt, TFun (TPrim PInt, TPrim PInt)))
    | Tuple t -> 
            let open Result in
            List.fold t ~init:(Ok ([], [])) ~f:(fun acc x ->
                acc
                >>= fun (sub, acc) ->
                    (infer env x) >>| fun (s2, x) ->
                        (s2 @ sub, x::acc))
            >>| fun (sub, types) -> (sub, TProd types)

