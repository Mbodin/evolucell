(** This file contains an alternative version of the programs, useful for user input as well as some global optimisations. **)

type state =
    (string * string list) option (** Optionnal name for the state and its arguments. This is only useful when printing the program to keep meaningful names. **)
    * register_type option array (** List of register arguments (indexed by their order in the array). The types are here optionnal to make the parser easily produce an object of this type, but the types are defined most of the time in practise. **)
    * instruction list (** The instructions applied in this state, in order **) (* TODO: we need a more complex syntax tree here. *)
    * int (** Index of the next state in the program array **)
    * expression array (** Arguments of the next state **)

(** The type of pretty-programs. Such programs are not meant to be executed, but serve as useful intermediary programs, making lists more useful than arrays here. **)
type t =
    program_state list

(** Type check whether a program is safe (returning the typed program), or returns the error if any. **)
val type_check : t -> (type_check_error, t) Utils.plus

(** Converts a pretty-program to a traditionnal one. **)
val convert_from : t -> Program.program

(** Converts a traditionnal to a pretty-program, limiting its number of state if possible. **)
val convert_to : Program.program -> t

(** Converts a traditionnal to a pretty-program, without any optimisation. The number of resulting state may be high. **)
val direct_translation : Program.program -> t

(** Fill out all optionnal state names of a program to make sure that all of them have a distinct name. **)
val name : t -> t

(** Converts to a list of string, one for each line (which is easier to print and to deal with indentation than a string). **)
val print : t -> string list

