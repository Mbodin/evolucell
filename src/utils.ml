
let _ = Random.self_init ()

let id x = x

let option_map f = function
    | None -> None
    | Some x -> Some (f x)

let if_option = function
    | None -> fun _ -> None
    | Some x -> fun f -> f x

type ('a, 'b) plus =
    | Left of 'a
    | Right of 'b

let error_monad o f =
    match o with
    | Left e -> Left e
    | Right v -> f v

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


type idt = int

let new_id_function () =
    let current = ref (-1) in
    fun () ->
        incr current ;
        !current

let new_id = new_id_function ()


module GeneralMap =
    Make.Make (struct
            let compare = Pervasives.compare
            type t = 'a
        end)

type _ idt_map =
    | Idt_map : idt GeneralMap.t * (unit -> idt) -> 'a idt_map
    | Idt_int : idt idt_map (* No need to create new identifiers for integers! *)

let idt_map_create =
    Idt_map (GeneralMap.empty, (new_id_function ()))

let idt_idt_map_create =
    Idt_int
let idt_int_map_create =
    idt_idt_map_create

let idt_map_insert_idt = function
    | Idt_map (m, f) -> fun o ->
        let i = f () in
        Idt_map (GeneralMap.add m o i, f), i
    | Idt_int -> fun i ->
        Idt_int, i

let idt_map_insert m e =
    fst (idt_map_insert_idt m e)

let get_id = function
    | Idt_map (m, f) -> fun o ->
        try Some (GeneralMap.find m o)
        with Not_found -> None
    | Idt_int -> fun i ->
        Some i


type 'a unionFind =
    'a idt_map * idt GeneralMap.t

let create_union_find = idt_map_create, idt_idt_map_create

let create_union_find_idt = idt_idt_map_create, idt_idt_map_create
let create_union_find_int = create_union_find_idt

let insert_idt (m, p) e =
    let (m, i) =
        match get_id m e with
        | None ->
            idt_map_insert_idt m e
        | Some i -> m, i
    in
    i, (m, GeneralMap.add p i i)

let insert mp e =
    snd (insert_idt mp e)

let find (m, p) e =
    let rec aux p i =
        let pi = GeneralMap.find p i in
        if i = pi then
            Some (i, p)
        else let (p, pi') = aux p pi in
             pi', GeneralMap.write p i pi'
    in
    if_option (GeneralMap.find m e) (aux p)

let find_insert mp e =
    match find mp e with
    | Some r -> r
    | None ->
        insert mp e

let merge mp e1 e2 =
    let (i1, mp) = find_insert mp e1 in
    let (i2, (m, p)) = find_insert mp e2 in
    m, GeneralMap.add p i1 i2

