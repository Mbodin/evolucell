
let _ = Random.self_init ()

let id x = x

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


module Compare =
    struct
        let compare = Pervasives.compare
        type t = 'a
    end

type _ idt_map =
    | Idt_map : idt Map.Make Compare.t -> (unit -> idt) -> 'a idt_map
    | Idt_int : idt idt_map (* No need to create new identifiers for integers! *)

let idt_map_create =
    Idt_map Map.Make Compare.empty (new_id_function ())

let idt_idt_map_create =
    Idt_int
let idt_int_map_create =
    Idt_int

let idt_map_insert = function
    | Idt_map m f -> fun o ->
        Idt_map (Map.Make Compare.add o (f ())) f
    | Idt_int -> fun _ -> Idt_int

let get_id = function
    | Idt_map m f -> fun o ->
        try Some (Map.Make (???).find m o)
        with Not_found -> None
    | Idt_int -> fun i -> Some i


type 'a unionFind =
    (* TODO *)

val create_union_find : unit -> 'a union_find

val insert : 'a union_find -> 'a -> unit

val find : 'a union_find -> 'a -> idt option

val merge : 'a union_find -> 'a -> 'a -> unit

