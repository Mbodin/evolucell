
type coordinate = Particle.coordinate

type entity =
  | Creature of coordinate * Program.program_state
  | SpecialCreature of coordinate * (unit -> TODO)
  | Particle of Particle.t
  | Nutrient of int * int

type cell =
  | Obstacle
  | Empty of entity list

type t =
  (int * int) (** Size **)
  * cell array array (** Map **)
  * entity list list (** List of entities for each execution step. **)


exception Non_rectangle_map

let initialise a =
  if Array.length a = 0 then
    ((0, 0), [||], [])
  else if array_for_all (fun v -> Array.length v = Array.length (a.(0))) a then
    ((Array.length a, Array.length (a.(0))),
     Array.map (Array.map (fun b -> if b then Obstacle else Empty [])) a,
     [])
  else raise Non_rectangle_map

exception Empty_map

let (mod) = Utils.positive_mod

let get_cell ((sx, sy), a, _) (x, y) =
  if sx <= 0 || sy <= 0 then raise Empty_map
  else a.(x mod sx).(y mod sy)

let set_cell ((sx, sy), a, _) (x, y) c =
  if sx <= 0 || sy <= 0 then raise Empty_map
  else a.(x mod sx).(y mod sy) <- c

exception Adding_entity_to_obstacle

let add_entity_to_cell c e =
  match c with
  | Obstacle -> raise Adding_entity_to_obstacle
  | Empty l -> Empty (e :: l)

(** When adding an entity, it is important (to know where to put it in the entity list) to know when it will then have to be executed. This function computes this. **)
let entity_initial_speed = function
  | Creature (_, st) -> TODO
  | Particle p -> Particle.speed p
  | Nutrient _ -> -1

let add_entity (size, a, l) coord e =
  let speed = entity_initial_speed e in
  let l =
    if speed >= 0 then
      TODO: Insert in the nth element of l
    else l in
  set_cell (size, a, l) coord (add_entity_to_cell (get_cell (size, a, l) coord) e) ;
  (size, a, l)

let add_creature t coord st =
  add_entity t coord (Creature (coord, st))


