
val max_integer (** = Particle.number - 1 **)

type register_type =
    | type_integer (** Between 0 and max_integer **)
    | type_direction
    | type_boolean

type expression =
    | Constant_integer of int (** Between 0 and max_integer included **)
    | Constant_direction of direction
    | Constant_boolean of bool
    | Register of int (** Index of the register **)

    | My_energy
    | My_life
    | Has_particle of expression (** Returns true if the given particle number is present in the current cell **)
    | Has_particles (** Returns true if the current cell contains particles **)
    | Cell_empty of direction (** Returns true if the indicated cell is not occupied, neither by a creature or by an obstacle **)
    | Cell_creature of direction (** Returns true if the indicated cell is occupied by a creature **)
    | Cell_obstacle of direction (** Returns true if the indicated cell is occupied by an obstacle **)

    | Equal of expression * expression (** Of any type, but they must be the same **)
    | Less_than of expression * expression (** Only integers **)
    | Addition of expression * expression (** Only integers; if the result is more than max_integer, it is wrapped modulo max_integer **)
    | Multiplication of expression * expression (** Only integers; if the result is more than max_integer, it is wrapped modulo max_integer **)
    | Substraction of expression * expression (** Only integers; if the result is less than 0, it is wrapped modulo max_integer **)
    | Division of expression * expression (** Only integers; if the second one is zero, the result is max_integer. **)
    | Modulo of expression * expression (** Only integers; if the second one is zero, the result is 0. **)
    | And of expression * expression (** Only booleans **)
    | Or of expression * expression (** Only booleans **)
    | Not of expression (** Only booleans **)
    | Opposite of expression (** Only direction **)
    | Turn_left of expression (** Only direction; turns 90 degrees left **)
    | Turn_right of expression (** Only direction; turns 90 degrees right **)

type instruction =
    | Jump_if of expression (** Of type boolean **)
        * int (** Index of the state in the program array **)
        * expression array (** Argument of the state **)
    | Move of expression (** The direction to move **)
    | Produce_particles of expression (** The particle number **)
        * expression (** The number of such particles **)
    | Collect_particles (** Collect all particles in the current cell, converting them into energy, but taking the eventual damages **)
    | Clone (** The creature clones itself, creating an offspring (mutations can happens at this stage) **)
        of expression (** The direction to be cloned **)
        * expression (** The energy given to the clone **)
        * int (** The initial state of the clone **)
        * expression array (** The arguments given to this initial state **)

type state =
    (string * string list) option (** Optionnal name for the state and its arguments. This is only useful when printing the program to keep meaningful names. **)
    * register_type array (** List of register arguments (indexed by their order in the array) **)
    * instruction (** The unique instruction of this state **)
    * int (** Cost of the instruction (we could compute it from the instruction, but it is more efficient to compute it once and for all) **)
    * int (** Index of the next state in the program array **)
    * expression array (** Arguments of the next state **)

(** Programs are seen as a finite state machine. **)
type program =
    program_state array


val instruction_cost : instruction -> int (** Returns the cost of the instruction **)

type type_check_error =
    | Invalid_number_of_argument of int (** Index of the calling state **)
        * int (** Expected number of arguments **)
        * int (** Given number of arguments **)
    | Invalid_type_of_argument of int (** Index of the calling state **)
        * int (** Index of the mistyped argument **)
        * register_type (** Expected type **)
        * register_type (** Given type **)
    | Invalid_state_index of int (** Index of the calling state **)

(** Type check whether an expression is safe (returning its type), or returns the error if any. **)
val type_check_expression : register_type array (** The registers of the current state **) -> expression -> (type_check_error, register_type) Utils.plus

(** Type check whether a program is safe (returning None), or returns the error if any. **)
val type_check : program -> type_check_error option

