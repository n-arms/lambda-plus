open Base
open Ast
open Sub
(*
module MT = Map.M(Int)
type substitution = mono_type MT.t

let subrule_of_list l =
    Map.of_alist_reduce (module Int) l ~f:(fun _ y -> y) 

let rec free_vars = function
    | Mono t -> (
        match t with
        | TVar n -> Set.singleton (module Int) n
        | TInt -> Set.empty (module Int)
        | TFun (l, r) -> Set.union (free_vars (Mono l)) (free_vars (Mono r)))
    | Poly (type_vars, m) ->
            Set.diff (free_vars (Mono m)) type_vars

let rec mono_sub s = function
    | TVar n -> Option.value (Map.find s n) ~default:(TVar n)
    | TFun (m, n) -> TFun (mono_sub s m, mono_sub s n)
    | t -> t

let rec poly_sub s = function
    | Mono m -> Mono (mono_sub s m)
    | Poly (type_vars, m) ->
            let s' = Map.filter_keys s ~f:(fun tv -> not (Set.mem type_vars tv)) in
            Poly (type_vars, mono_sub s' m)

let (@@) s2 s1 =
    Map.merge s1 s2 ~f:(fun ~key:_ -> function
        | `Both (_, n) | `Right n -> Some (mono_sub s2 n)
        | `Left m -> Some m)


let env_of_list l =
    Map.of_alist_reduce (module Int) l ~f:(fun _ y -> y)

let env_replace (var_name, poly) env =
    Map.set env ~key:var_name ~data:poly

let free_in_env env =
    Map.fold env ~init:(Set.empty (module Int)) ~f:(fun ~key:_ ~data:t fv -> Set.union fv (free_vars t))

let sub_in_env sub env =
    Map.map env ~f:(poly_sub sub)

let generalize env m = 
    Poly (Set.diff (free_vars (Mono m)) (free_in_env env), m)

let fresh_type_var =
    let count = ref (-1) in
    (fun () -> count := !count + 1; !count)

let inst = function
    | Poly (type_vars, m) ->
            let sub = Set.fold type_vars ~f:(fun acc type_var -> Map.add_exn acc ~key:type_var ~data:(TVar (fresh_type_var()))) ~init:(Map.empty (module Int)) in
            mono_sub sub m
    | Mono m -> m

let rec occurs type_var = function
    | TVar n -> n = type_var
    | TInt -> false
    | TFun (l, r) -> (occurs type_var l) || (occurs type_var r)

let rec unify m1 m2 =
    match m1, m2 with
    | TInt, TInt ->
            Some (Map.empty (module Int))
    | TVar n1, _ when not (occurs n1 m2) -> Some (subrule_of_list [(n1, m2)])
    | _, TVar n2 when not (occurs n2 m1) -> Some (subrule_of_list [(n2, m1)])
    | TFun (l1, r1), TFun (l2, r2) ->
            let open Option in
            (unify l1 l2) 
            >>= (fun s1 -> 
                let s = (mono_sub s1) in
                (unify (s r1) (s r2))
                >>| (fun s2 -> (s2 @@ s1)))
    | _ -> None

let rec infer env = 
    let open Option in
    function
    | Arg a ->
            (Map.find env a)
            >>| (fun m -> (Map.empty (module Int), inst m))
    | Num _ ->
            Some (Map.empty (module Int), TInt)
    | Func (a, e) ->
            let fresh_tv = TVar (fresh_type_var()) in
            (infer (env_replace (a, Mono fresh_tv) env ) e)
            >>| (fun (sub, mono) -> (sub, mono_sub sub (TFun (fresh_tv, mono))))
    | App (l, r) ->
            (infer env l)
            >>= (fun (s1, m1) ->
                (infer (sub_in_env s1 env) r)
                >>= (fun (s2, m2) ->
                    let fresh_tv = TVar (fresh_type_var()) in
                    (unify (mono_sub s2 m1) (TFun (m2, fresh_tv)))
                    >>| (fun s3 ->
                        (s3 @@ s2 @@ s1, mono_sub s3 fresh_tv))))
    | Let (a, e, b) ->
            (infer env e)
            >>= (fun (s1, m1) ->
                let s1env = sub_in_env s1 env in
                (infer (env_replace (a, generalize s1env m1) s1env) b)
                >>| (fun (s2, m2) -> (s2 @@ s1, m2)))
    | _ -> None

let rec infer' env = 
    let open Option in
    function
    | Num _ -> Some (Map.empty (module Int), TInt)
    | Arg a -> 
            (Map.find env a)
            >>| fun t -> (Map.empty (module Int), inst t)
    | App (l, r) -> 
            (infer' env l)
            >>= (fun (s0, t0) ->
                (infer' env r)
                >>= (fun (s1, t1) ->
                    let fresh_tv = TVar (fresh_type_var ()) in
                    (unify (mono_sub s1 t0) (TFun (t1, fresh_tv)))
                    >>| fun s2 ->
                        s2 @@ s1 @@ s0, mono_sub s2 fresh_tv))
                    
    | Func (a, e) -> 
            let fresh_tv = TVar (fresh_type_var()) in
            (infer (env_replace (a, Mono fresh_tv) env ) e)
            >>| (fun (sub, mono) -> (sub, mono_sub sub (TFun (fresh_tv, mono))))
    | Let (a, e, b) -> 
            (infer env e)
            >>= (fun (s1, m1) ->
                let s1env = sub_in_env s1 env in
                (infer (env_replace (a, generalize s1env m1) s1env) b)
                >>| (fun (s2, m2) -> (s2 @@ s1, m2)))
    | Op o -> None*)


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
    | Let (x, e1, e2) -> 
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
    | Op _ -> Some ([], TFun (TInt, TFun (TInt, TInt)))
