
(** The sum type of two types. **)
type ('a, 'b) plus =
    | Left of 'a
    | Right of 'b

