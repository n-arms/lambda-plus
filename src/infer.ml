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
                | TInt -> Set.empty (module Int))
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
    | TInt, TInt -> Some []
    | TVar n1, _ when not (occurs n1 m2) -> Some [n1, m2]
    | _, TVar n2 when not (occurs n2 m1) -> Some [n2, m1]
    | TFun (m11, m12), TFun (m21, m22) ->
            let open Option in
            (unify m11 m21)
            >>= fun s1 ->
                let s = apply_mono s1 in
                (unify (s m12) (s m22))
                >>| fun s2 ->  s2 @ s1
    | _ -> None

let rec infer (env : env) : expr -> (sub * mono_type) option = function
    | Arg x -> 
            let open Option in
            (Map.find env x)
            >>| fun t -> ([], inst t)
    | Num _ -> Some ([], TInt)
    | Func (x, e1) -> 
            let open Option in
            let t = TVar (newvar ()) in
            (infer (Map.set env ~key:x ~data:(Mono t)) e1)
            >>| fun (s, t') -> (s, TFun (apply_mono s t, t'))
    | Let (x, e1, e2) | LetRec (x, e1, e2) -> 
            let open Option in
            (infer env e1)
            >>= fun (s0, t) ->
                (infer (Map.set env ~key:x ~data:(generalize (apply_over_env s0 env) t)) e2) (* XXX env should be s0 (env) *)
                >>| fun (s1, t') ->
                    (s1 @ s0, t')
    | App (e1, e2) -> 
            let open Option in
            (infer env e1)
            >>= fun (s0, t0) ->
                (infer (apply_over_env s0 env) e2)
                >>= fun (s1, t1) ->
                    let t' = TVar (newvar ()) in
                    (unify (apply_mono s1 t0) (TFun (t1, t')))
                    >>| fun s2 ->
                        (s2 @ (s1 @ s0), apply_mono s2 t')
    | Op UnaryMinus -> Some ([], TFun (TInt, TInt))
    | Op Fix -> 
            let t = TVar (newvar ()) in
            Some ([], TFun (TFun (t, t), t))
    | Op _ -> Some ([], TFun (TInt, TFun (TInt, TInt)))
