open Base

let rec join_with_commas f = function
    | h1::h2::tl -> (f h1)^", "^(join_with_commas f (h2::tl))
    | [hd] -> (f hd)
    | [] -> ""

let rec unfold ~init ~f =
    let open Option in
    f init
    >>| (fun (hd, acc) ->
        hd::(unfold ~init:acc ~f:f))
    |> value ~default:[]

