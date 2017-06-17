
let _ = Random.self_init ()

let id x = x

type ('a, 'b) plus =
    | Left of 'a
    | Right of 'b

let rec unfold f i =
    match f i with
    | None -> []
    | Some (v, j) ->
        v :: unfold f j

let seq i =
    (* We assume i >= 0. *)
    unfold (fun j ->
        if i = j then None
        else Some (j, j + 1)) 0

exception NegativeWeigth
exception InternalError

let select l =
    let s = List.fold_left (+) 0 (List.map fst l) in
    if s <= 0 then raise NegativeWeigth
    else
        let rec search t = function
            | [] -> raise InternalError
            | (p, v) :: l ->
                if p >= t then v
                else search (t - p) l
        in search (Random.int s) l


let sum = List.fold_left (+) 0
let array_sum = Array.fold_left (+) 0

let array_count f = Array.fold_left (fun v x -> v + if f x then 1 else 0) 0

