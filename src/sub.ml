open Base

type sub = (Ast.type_var_name * Ast.mono_type) list
type env = Ast.poly_type Map.M(Int).t

let find sub tvn = 
    List.fold sub ~init:None ~f:(fun acc (t, m) -> Option.first_some acc (if t = tvn then Some m else None))

let find_exn sub tvn = 
    Option.value_exn (find sub tvn) ~message:("failed to find type var "^(Int.to_string tvn))

let mem sub tvn = 
    (find sub tvn)
    |> Option.map ~f:(fun _ -> true)
    |> Option.value ~default:false

let map sub f = 
    List.map sub ~f:(fun (t, m) -> (t, f m))

let filter sub p =
    List.filter sub ~f:(fun (t, _) -> p t)

let rec apply_mono sub = function
    | Ast.TVar n ->
            (find sub n)
            |> Option.value ~default:(Ast.TVar n)
    | Ast.TInt as i -> i
    | Ast.TFun (m1, m2) -> Ast.TFun (apply_mono sub m1, apply_mono sub m2)

let apply_poly sub = function
    | Ast.Poly (tvns, m) -> 
            let sub2 = filter sub (fun k -> not (Set.mem tvns k)) in
            Ast.Poly (tvns, apply_mono sub2 m)
    | Ast.Mono m -> Mono (apply_mono sub m)

let apply_over_env sub env = 
    Map.map env ~f:(apply_poly sub)

let string_of_env env =
    let rec join_with_commas f = function
        | h1::h2::tl -> (f h1)^", "^(join_with_commas f (h2::tl))
        | [hd] -> (f hd)
        | [] -> "" in
    Map.fold env ~init:[] ~f:(fun ~key:k ~data:d acc -> ((Ast.decode_arg k)^" => "^(Ast.string_of_poly_type d))::acc)
    |>  join_with_commas (fun x -> x)
    |> fun s -> "{"^s^"}"
