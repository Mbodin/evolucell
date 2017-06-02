
val number : int (** The number of different particles in the game: 1000. **)

(** The following functions take an integer as argument. This integer is the index of the referred particle, from 0 to number - 1. No other number should be given to this function. **)

val active_damage : int -> int (** The number of damage the particle causes when absorbing it. **)
val passive_damage : int -> int (** The number of damage the particle causes at each turn, when a cell stands at the same place than the particle. **)
val active_heal : int -> int (** The number of life point the particle regenerate when absording it. **)
val passive_heal : int -> int (** The number of life point the particle regenerate at each turn, when a cell stands at the same place than the particle. **)
val light_sensitive : int -> bool (** Whether the particle can absord light, making it able to store energy. **)
val boost_nutrients : int -> bool (** Whether the particle can boost nutrients, making creatures absorb more energy when absorbing then. **)
val speed : int -> int * int (** The number of turn the particle waits before moving and the number of successive turn it then moves. The possible values are from increasing speed (4, 1); (3, 1); (2, 1); (1, 1); (1, 3); (1, 2); (0, 1). The value (0, 1) is maximum and means that the particle moves at each turn. The value (1, 1) means that the particle moves one turn, then waits one turn, and so on. The value (1, 2) means that the particle moves at each turn except once every three turns. **)

val check_reaction : int -> int -> int option (** If reaction p1 p2 = None, then the particles p1 and p2 do not react with each other. If reaction p1 p2 = Some p3, this means that when in the same cell, particle p1 will spontaneously transform into particle p3. **)
val reaction : int -> (int * int) option (** Any particle type has at most one other particle type with which it interacts. This function returns for each particle type a pair of the optionnal particule with which it interacts, and the resulting particle. **)

val cost_to_emit : int (** Energy cost to emit a particle. **)
val gain_when_absorbed : int (** Energy gained when absorbing a particle. **)
(** Note that gain_when_absorbed < cost_to_emit: we lose some energy in the process of creating and absording particles. **)

type t = (** Type of a given particle released in the field. **)
    int (** Particle index (from 0 to number - 1) **)
    * (int * int) (** Coordinates in the field **)
    * int (** Additional energy stored in the particle due to light absorbtion **)

