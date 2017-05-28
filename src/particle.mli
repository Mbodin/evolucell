
val number (** The number of different particles in the game. **)

(** The following functions take an integer as argument. This integer is the index of the referred particle, from 0 to number -1. No other number should be given to this function. **)

val active_damage : int -> int (** The number of damage the particle causes when absorbing it. **)
val passive_damage : int -> int (** The number of damage the particle causes at each turn, when a cell stands at the same place than the particle. **)
val active_curation : int -> int (** The number of life point the particle regenerate when absording it. **)
val passive_curation : int -> int (** The number of life point the particle regenerate at each turn, when a cell stands at the same place than the particle. **)
val speed : int -> int (** The number of turn the particle waits before moving: 1 is maximum and means that the particle moves at each turn. **)
val absord_lights : int -> bool (** Whether the particle can absord light, making it able to store energy. **)

val reaction : int -> int -> int option (** If reaction p1 p2 = None, then the particles p1 and p2 do not react with each other. If reaction p1 p2 = Some p3, this means that when in the same cell, particle p1 will spontaneously transform into particle p3. **)

