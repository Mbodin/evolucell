
type cell

type t =
    cell array array (** Map **)
    * (int * int) list (** List of creature coordonates. **)
    * unit list (** TODO: List of particles to move. **)

(** Indicates whether a cell is empty, that is that neither creatures neither obstacles are in the cell. **)
val is_empty : cell -> bool

(** Checks whether a cell has an obstacle (if so, nothing else can be in the cell). **)
val has_obstacle : cell -> bool

(** Indicates whether a cell has a creature in it. **)
val has_creature : cell -> bool

(** Indicates whether a cell has particles in it. **)
val has_particles : cell -> bool

(** Indicates whether a cell has the given particle in it. **)
val has_particle : cell -> int -> bool

