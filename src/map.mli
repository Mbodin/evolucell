
type cell

(** The type entity records all the movable components of the map. **)
type entity =
  | Creature of int * int (** Creature coordinates. **)
  | Particle (* TODO *)
  | Approvision

type t =
  cell array array (** Map **)
  * entity list (** List of entities **)

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

