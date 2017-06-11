
let max_integer = Particle.number - 1

type register_type =
    | Type_integer
    | Type_direction
    | Type_boolean
    | Type_state of register_type list
    | Type_any

type direction =
    | N
    | E
    | S
    | W

type value =
    | Value_integer of int
    | Value_direction of direction
    | Value_boolean of bool
    | Value_state
        of int
        * value list

type expression =
    | Constant_integer of int
    | Constant_direction of direction
    | Constant_boolean of bool
    | Constant_state of int
    | Register of int

    | My_energy
    | My_life
    | Has_nutrients
    | Has_particle of expression * expression
    | Has_particles
    | Cell_empty of direction
    | Cell_creature of direction
    | Cell_obstacle of direction

    | Random of expression
    | Random_direction
    | Random_boolean

    | Equal of expression * expression
    | Less_than of expression * expression
    | Addition of expression * expression
    | Multiplication of expression * expression
    | Substraction of expression * expression
    | Division of expression * expression
    | Modulo of expression * expression
    | And of expression * expression
    | Or of expression * expression
    | Not of expression
    | Opposite of expression
    | Turn_left of expression
    | Turn_right of expression
    | Apply of expression * expression list

type instruction =
    | Wait
    | Jump
        of expression
    | Jump_if
        of expression
        * int
        * expression array
    | Move
        of expression
    | Produce_particles
        of expression
        * expression
    | Collect_particles
    | Produce_nutrient
        of expression
    | Consume_nutrient
    | Clone
        of expression
        * expression
        * int
        * expression array

type state =
    * register_type array
    * instruction
    * int
    * int
    * expression array

type program =
    state array

type program_state =
    program
    * int
    * value array

let expression_cost = function
    | Constant_integer _ -> 0
    | Constant_direction of direction
    | Constant_boolean of bool
    | Constant_state of int
    | Register of int

    | My_energy
    | My_life
    | Has_nutrients
    | Has_particle of expression * expression
    | Has_particles
    | Cell_empty of direction
    | Cell_creature of direction
    | Cell_obstacle of direction

    | Random of expression
    | Random_direction
    | Random_boolean

    | Equal of expression * expression
    | Less_than of expression * expression
    | Addition of expression * expression
    | Multiplication of expression * expression
    | Substraction of expression * expression
    | Division of expression * expression
    | Modulo of expression * expression
    | And of expression * expression
    | Or of expression * expression
    | Not of expression
    | Opposite of expression
    | Turn_left of expression
    | Turn_right of expression
    | Apply of expression * expression list

let instruction_cost = function
    | Wait -> 1
    | Jump
        of expression
    | Jump_if
        of expression
        * int
        * expression array
    | Move
        of expression
    | Produce_particles
        of expression
        * expression
    | Collect_particles
    | Produce_nutrient
        of expression
    | Consume_nutrient
    | Clone
        of expression
        * expression
        * int
        * expression array

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

