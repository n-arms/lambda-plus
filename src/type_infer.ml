open Base
open Expr
open Typed_expr

let type_of te =
    match te with
    | ANum _ -> TEInt
    | AFunc (_, _, t) -> t
    | AApp (_, _, t) -> t
    | AArg (_, t) -> t
    | AOp _ -> TEArrow (TEInt, TEInt)

let code = ref (64)
let next_type_var () = code := !code + 1; String.of_char (Char.of_int_exn !code)
let reset_type_vars () = code := 64

let annotate e =
    let (h: (string, expr_type) Hashtbl.t) = Hashtbl.create (module String) in
    let rec annotate' e bv =
        match e with
        | Arg x ->
            (* is x inside of bv (bound variables *)
            (try let a = List.Assoc.find_exn bv ~equal:String.equal x  in AArg (x, a)
            (* is x a known free var *)
            with Not_found_s _ -> try let a = Hashtbl.find_exn h x in AArg (x, a)
            (* unknown free var *)
            with Not_found_s _ -> let a = TEGeneric (next_type_var ()) in Hashtbl.set h ~key:x ~data:a; AArg (x, a))
        | Func (x, e) ->
                let a = TEGeneric (next_type_var()) in
                let te = annotate' e ((x, a) :: bv) in
                AFunc (x, te, TEArrow (a, type_of te))
        | App (e1, e2) -> 
                AApp (annotate' e1 bv, annotate' e2 bv, TEGeneric (next_type_var()))
        | Num n ->
                ANum n
        | Op o ->
                AOp o

    in annotate' e []

let rec collect typed_exprs u =
    match typed_exprs with
    | [] -> u
    | AArg (_, _) :: r -> collect r u
    | AFunc (_, te, _) :: r -> collect (te :: r) u
    | AApp (te1, te2, t) :: r ->
            let (f, b) = (type_of te1, type_of te2) in
            collect (te1 :: te2 :: r) ((f, TEArrow(b, t)) :: u)
    | ANum _ :: r -> collect r u
    | AOp _ :: r -> collect r u

let infer_type e =
    reset_type_vars();
    let te = annotate e in
    let cl = collect [te] [] in
    let s = Type_unify.unify cl in
    Type_unify.apply s (type_of te)
