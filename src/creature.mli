
val life (** Total life of creatures **)

type t =
  int (** Current damage that the cell has experienced. If damage > life, the cell is dead. **)
  * int (** Current energy the cell has. **)
  * int (** Current delay of a cell, which is removed from the time it can compute at this turn. **)

(** At each turn, the cell’s program is executed. When it reaches the maximal number of executed steps during a turn (usually, 10), or when it produces a side effect (typically, moving), the cell is stopped. However, as some instructions uses more than one step at a time (typically because of complex inner expressions), the cell actually used more than this limit this turn. These extra steps then converts into a delay for the next turn: the cell is considered to have executed a number of step equal to its current delay. **)

