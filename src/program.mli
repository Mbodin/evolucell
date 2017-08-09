
(** The maximum integer that can be stored in a register. **)
val max_integer : int (** = Particle.number - 1 **)

type type_variable (** The type of type variables **)
type type_state_variable (** The type of type variables whose type is an application whose final result is a state **)
type state_index (** The type of state indexes **)
type register_index (** The type of register indexes **)

type register_type = (** The type of registers **)
  | Type_integer (** Between 0 and max_integer **)
  | Type_direction (** Cardinal direction **)
  | Type_boolean (** A boolean **)
  | Type_fun_state of register_state_type (** The type of a state (wich may take some paramters) **)
  | Type_variable of type_variable (** A type variable, mainly used during the type-checking. **)

and register_state_type = (** Some registers have are states (with parameters), here follows their type **)
  | Type_state (** The type of a fully applied state **)
  | Type_fun of register_type * register_state_type (** The type of a state with an argument **)
  | Type_state_variable of type_state_variable (** A type variable, which can only be instanciated by a state-type **)

type direction =
  | N (** North **)
  | E (** East **)
  | S (** South **)
  | W (** West **)

type value = (** The type of program values **)
  | Value_integer of int (** Between 0 and max_integer included **)
  | Value_direction of direction
  | Value_boolean of bool
  | Value_state
    of state_index (** Index of the state in the program array **)
    * value list (** Partically applied arguments **)

type expression =
  | Constant_integer of int (** Between 0 and max_integer included **)
  | Constant_direction of direction
  | Constant_boolean of bool
  | Constant_state of state_index (** Index of the state in the program array **)
  | Register of register_index (** Index of the register **)

  | My_energy
  | My_life
  | Has_nutrients (** Returns true if there are nutrients in the current cell **)
  | Has_particle of expression * expression (** Returns true if a particle between the two bounds is present in the current cell **)
  | Has_particles (** Returns true if the current cell contains particles **)
  | Cell_empty of expression (** Returns true if the indicated cell is not occupied, neither by a creature or by an obstacle **)
  | Cell_creature of expression (** Returns true if the indicated cell is occupied by a creature **)
  | Cell_obstacle of expression (** Returns true if the indicated cell is occupied by an obstacle **)

  | Random of expression (** Produces a random number between 0 and the given expression, which must be a number **)
  | Random_direction (** Produces a random direction **)
  | Random_boolean (** Produces a random boolean **)

  | If_then_else of expression * expression * expression (** The first must be a boolean, the other two of any type, but must be the same **)
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
    * state_index (** Index of the state in the program array **)
    * expression array (** Argument of the state **)
  | Move (* The creature moves in the following direction. If there is an obstacle, the creature doesn’t move and gets some damages. If there is another creature, both creatures take damages. *)
    of expression (** The direction to move **)
  | Produce_particles (** Produces some particles. Note that there is a maximum number on the number of particle produced each turn. If the creature requires to produce more than possible, only the maximum amount is produced. **)
    of expression (** The particle number **)
    * expression (** The number of such particles **)
  | Collect_particles (** Collect all particles in the current cell, converting them into energy, but taking the eventual damages **)
  | Produce_nutrient (** Produce the equivalent amount uf energy given in argument to nutrients **)
    of expression (** The energy given **)
  | Consume_nutrient (** Consume the nutrients present in the current cell, if any **)
  | Clone (** The creature clones itself, creating an offspring (mutations can happens at this stage) **)
    of expression (** The direction to be cloned **)
    * expression (** The energy given to the clone **)
    * state_index (** The initial state of the clone **)
    * expression array (** The arguments given to this initial state **)

type state =
  (string * string list) option (** Optional name for the state and its arguments. This is only useful when printing the program to keep meaningful names. **)
  * register_type array (** List of register arguments (indexed by their order in the array) **)
  * instruction (** The unique instruction of this state **)
  * state_index (** Index of the next state in the program array **)
  * expression array (** Arguments of the next state **)

(** Programs are seen as a finite state machine. **)
type program =
  state array

(** A program state is composed of the whole program and the current state it currently is.  **)
type program_state =
  program
  * state_index (** Current state number **)
  * value array (** Arguments given to the state **)

val expression_cost : expression -> int (** Returns the cost of the expression. **)

val instruction_cost : instruction -> int (** Returns the cost of the instruction. **)

(** Possible type check errors. **)
type type_check_error =
  | Invalid_argument_index
    of state_index (** Index of the calling state **)
    * int (** Number of declared arguments **)
    * register_index (** Index of the non-existing register **)
  | Invalid_number_of_argument
    of state_index (** Index of the calling state **)
    * int (** Expected number of arguments **)
    * int (** Given number of arguments **)
  | Invalid_type_of_argument
    of state_index (** Index of the calling state **)
    * register_index (** Index of the mistyped argument **)
    * register_type (** Expected type **)
    * register_type (** Given type **)
  | Invalid_state_index of state_index (** Index of the calling state **)
  | Incompatible_types
    of state_index (** Index of the calling state **)
    * register_type * register_type (** The two incompatible types **)

(** A structure to type programs. It associates each variable to its inferred type. **)
type ('var, 't) type_map =
  'var Utils.union_find (** Each variable is associated to a type identifier, and these identifiers can be merged. **)
  * (Utils.idt, 't) PMap.t (** Each representant of type identifier (that is, the potential results of Utils.find on the previous structure) are associated a type. **)

(** Two aliases of the type. **)
type variable_type_map = (type_variable, register_type) type_map
type variable_state_type_map = (type_state_variable, register_state_type) type_map

(** Add a variable to the typing state, associating it to itself. **)
val add_variable : type_variable -> variable_type_map -> variable_type_map
val add_state_variable : type_state_variable -> variable_state_type_map -> variable_state_type_map

(** The current global state of the typing process **)
type typing_state =
  register_type array array (** The types of the arguments of each state **)
  * variable_type_map (** Type map for variables **)
  * variable_state_type_map (** Type map for type variables **)

(** Initialises the typing state, associating each type variable to itself. **)
(* FIXME: Is this really what its signature should be? *)
val init_typing_state : type_variable list -> type_state_variable list -> typing_state

(** Merges the types in the current typing state. In case of an error, it returns the two incompatible types. **)
val merge_types : typing_state -> register_type -> register_type -> (register_type * register_type, typing_state) Utils.plus

(** Type check an expression (returning its type), or returns an error with an expression which type checks if possible. **)
val type_check_expression : typing_state -> state_index (** The current state index **) -> expression -> (type_check_error * (typing_state * expression * register_type) option, typing_state * register_type) Utils.plus

(** Type check an instruction (whose type should be state), or returns an error with an instruction which type checks if possible. **)
val type_check_instruction : typing_state -> state_index (** The current state index **) -> instruction -> (type_check_error * (typing_state * instruction) option, typing_state) Utils.plus

(** Type check whether a program is safe (returning None), or returns an error with a program which type checks. **)
val type_check : program -> (type_check_error * program) option

(** Actions / side effects of a program **)
type action =
  | Action_wait
  | Action_move of direction
  | Action_produce_particules of int (** Particle number **)
  | Action_collect_particules
  | Action_produce_nutrient of int (** Quantity of energy converted **)
  | Action_consume_nutrient
  | Action_clone
    of direction (** Direction to clone **)
    * int (** Energy given to the new creature **)
    * program_state (** Program state of the new creature **)

(** Execute one instruction of a program **)
val execute : program -> program_state -> action option * program_state

(** Iterate the execution a given number of time. If no action is emitted when the bound is reached, the chosen action is Action_wait. **)
val execute_several : int (** Time given **) -> program -> program_state -> action * program_state

