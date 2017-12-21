
(** The type for levels **)
type t

(** Languages **)
type language =
  | En (** English **)
  | Eo (** Esperanto **)

(** The type of language-dependent strings. **)
type lstring = language -> string

(** Groups of levels are groups together (for the tutorial, easy level, etc.), then grouped together as this list of levels. **)
type group =
  lstring (** Group name **)
  * t list (** Levels **)

val levels = group list

