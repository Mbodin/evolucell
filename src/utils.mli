
(** The sum type of two types. **)
type ('a, 'b) plus =
    | Left of 'a
    | Right of 'b


(** Creates a list from a function providing the optionnal next element and iterator. The first element given is used to initialise the function and is not inserted into the list. **)
val unfold : ('a -> ('b * 'a) option) -> 'a -> 'b list

(** Builds the list of nth first elements, from 0 to n - 1. **)
val seq : int -> int list

(** Takes a weigthed list and return a random element from it. **)
val select : (int * 'a) list -> 'a

(** Possible exception returned by the select function. **)
exception NegativeWeigth
(** Note that if the list is empty, the total weigth will be zero and the exception NegativeWeigth will also be sent. **)
exception InternalError

