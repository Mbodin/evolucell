
(** The sum type of two types. **)
type ('a, 'b) plus =
    | Left of 'a
    | Right of 'b


(** Creates a list from a function providing the optionnal next element and iterator. **)
let unfold : ('a -> ('b * 'a) option) -> 'a -> list 'b

(** Builds the list of nth first elements, from 0 to n - 1. **)
let seq : int -> int list

(** Takes a weigthed list and return a random element from it. **)
let select : (int * 'a) list -> 'a

(** Possible exceptions returns by the select function. **)
exception NegativeWeigth
exception EmptyList

