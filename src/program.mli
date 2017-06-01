
val max_integer (** = Particle.number - 1 **)

type register_type =
    | type_integer (** Between 0 and max_integer **)
    | type_direction (** Cardinal direction **)
    | type_boolean (** A boolean **)
    | type_state of register_type list (** A state, which can be called later on. The state can be partially applied: the list corresponds to the yet unapplied arguments. **)
    | type_any (** For unused arguments. Any object can be given to them. They may be optimised out. **)

type expression =
    | Constant_integer of int (** Between 0 and max_integer included **)
    | Constant_direction of direction
    | Constant_boolean of bool
    | Constant_state of int (** Index of the state in the program array **)
    | Register of int (** Index of the register **)

    | My_energy
    | My_life
    | Has_nutrients (** Returns true if there are nutrients in the current cell **)
    | Has_particle of expression * expression (** Returns true if a particle between the two bounds is present in the current cell **)
    | Has_particles (** Returns true if the current cell contains particles **)
    | Cell_empty of direction (** Returns true if the indicated cell is not occupied, neither by a creature or by an obstacle **)
    | Cell_creature of direction (** Returns true if the indicated cell is occupied by a creature **)
    | Cell_obstacle of direction (** Returns true if the indicated cell is occupied by an obstacle **)

    | Random of expression (** Produces a random number between 0 and the given expression, which must be a number **)
    | Random_direction (** Produces a random direction **)
    | Random_boolean (** Produces a random boolean **)

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
    | Apply of expression * expression list (** Apply a state to its arguments. This may be a partial application. This returns a value of type state, with some optionnal argument if the application was partial. **)

type instruction =
    | Wait (** Does nothing during one turn **)
    | Jump (** Jump to the dynamic state **)
        of expression (** The state to jump to. It must be of type state with no argument (that is, be a fully applied state). **)
    | Jump_if (** Jump to the following state if the computed expression evaluates to true **)
        of expression (** Of type boolean **)
        * int (** Index of the state in the program array **)
        * expression array (** Argument of the state **)
    | Move (* The creature moves in the following direction. If there is an obstacle, the creature doesn’t move and gets some damages. If there is another creature, both creatures take damages. *)
        of expression (** The direction to move **)
    | Produce_particles (** Produces some particles. Note that there is a maximum number on the number of particle produced each turn. If the creature requires to produce more than possible, only the maximum amount is produced. **)
        of expression (** The particle number **)
        * expression (** The number of such particles **)
    | Collect_particles (** Collect all particles in the current cell, converting them into energy, but taking the eventual damages **)
    | Consume_nutrient (** Consume the nutrients present in the current cell, if any **)
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

