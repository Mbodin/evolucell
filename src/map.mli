
(** The map type. **)
type t

(** The cell type. **)
type cell

type coordinate = Particle.coordinate

(** The type entity records all the movable components of the map. **)
type entity =
  | Creature of coordinate * program_state
  | Particle of Particle.t
  | Nutrient of int (** Energy currently available in the nutrient **) * int (** Additional energy that may be converted into a directly usuable one using particles. **)

(** Get the cell at this coordinate in the map. **)
val get_cell : t -> coordinate -> cell

(** Indicates whether a cell is empty, that is that neither creatures neither obstacles are in the cell. **)
val is_empty : cell -> bool

(** Checks whether a cell has an obstacle (if so, nothing else can be in the cell). **)
val has_obstacle : cell -> bool

(** Indicates whether a cell has a creature in it. **)
val has_creature : cell -> bool

(** Indicates whether a cell has nutriments in it. **)
val has_nutriment : cell -> bool

(** Indicates whether a cell has particles in it. **)
val has_particles : cell -> bool

(** Indicates whether a cell has the given particle in it. **)
val has_particle : cell -> int -> bool

(** Indicates whether a cell has one of particle between the two (inclusive) bounds. This check is done in increasing order: particle_set c a b when b < a checks whether there are particles after a or before b. **)
val has_particle_set : cell -> int -> int -> bool

(** Creates an empty map from a map of obstacle. **)
val initialise : bool (** If it is an obstacle **) array array -> t

(** Add a creature to the map. **)
val add_creature : t -> coordinate -> program_state -> t

(** Add a nutrient to the map. **)
val add_nutrient : t -> coordinate -> int (** Energy currently available **) -> int (** Additional energy **) -> t

(** Add a particle to the map. **)
val add_particle : t -> coordinate -> Particle.t -> t

(** Execute one step of all what needs to be moved on the map. **)
val execute : t -> (entity -> TODO) -> t

