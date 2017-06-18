
let max_integer = Particle.number - 1

type register_type =
    | Type_integer
    | Type_direction
    | Type_boolean
    | Type_state of register_type list
    | Type_variable of Utils.idt

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

    | If_then_else of expression * expression * expression
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

let rec expression_cost = function
    | Constant_integer _ -> 0
    | Constant_direction _ -> 0
    | Constant_boolean _ -> 0
    | Constant_state _ -> 0
    | Register _ -> 0

    | My_energy -> 0
    | My_life -> 0
    | Has_nutrients -> 1
    | Has_particle (e1, e2) ->
        1 + expression_cost e1 + expression_cost e2
    | Has_particles -> 1
    | Cell_empty e ->
        1 + expression_cost e
    | Cell_creature e ->
        1 + expression_cost e
    | Cell_obstacle e ->
        1 + expression_cost e

    | Random e ->
        1 + expression_cost e
    | Random_direction -> 1
    | Random_boolean -> 1

    | If_then_else (e1, e2, e3) ->
        1 + expression_cost e1 + max (expression_cost e2) (expression_cost e3)
    | Equal (e1, e2) ->
        1 + expression_cost e1 + expression_cost e2
    | Less_than (e1, e2) ->
        1 + expression_cost e1 + expression_cost e2
    | Addition (e1, e2) ->
        1 + expression_cost e1 + expression_cost e2
    | Multiplication (e1, e2) ->
        1 + expression_cost e1 + expression_cost e2
    | Substraction (e1, e2) ->
        1 + expression_cost e1 + expression_cost e2
    | Division (e1, e2) ->
        1 + expression_cost e1 + expression_cost e2
    | Modulo (e1, e2) ->
        1 + expression_cost e1 + expression_cost e2
    | And (e1, e2) ->
        1 + expression_cost e1 + expression_cost e2
    | Or (e1, e2) ->
        1 + expression_cost e1 + expression_cost e2
    | Not e ->
        1 + expression_cost e
    | Opposite e ->
        1 + expression_cost e
    | Turn_left e ->
        1 + expression_cost e
    | Turn_right e ->
        1 + expression_cost e
    | Apply (e, l) ->
        1 + expression_cost e + Utils.sum (List.map expression_cost e)

let instruction_cost = function
    | Wait -> 1
    | Jump e ->
        expression_cost e
    | Jump_if (e, _, a) ->
        1 + expression_cost e + Utils.array_sum (Array.map expression_cost a)
    | Move e ->
        1 + expression_cost e
    | Produce_particles (e1, e2) ->
        1 + expression_cost e1 + expression_cost e2
    | Collect_particles -> 1
    | Produce_nutrient e ->
        1 + expression_cost e
    | Consume_nutrient -> e
    | Clone (e1, e2, _, a) ->
        1 + expression_cost e1 + expression_cost e2 + Utils.array_sum (Array.map expression_cost a)

type type_check_error =
    | Invalid_number_of_argument
        of int
        * int
        * int
    | Invalid_type_of_argument
        of int
        * int
        * register_type
        * register_type
    | Invalid_state_index of int

(** Type check whether an expression is safe (returning its type), or returns the error if any. **)
let rec type_check_expression registers = function
    | Constant_integer _ ->
        Utils.right Type_integer
    | Constant_direction _ ->
        Utils.right Type_direction
    | Constant_boolean _ ->
        Utils.right Type_boolean
    | Constant_state _ ->
        Utils.right Type_state (* TODO: We need to know the arguments of this state… *)
    | Register i ->
        if i < 0 or i >= Array.length registers then
            Utils.left (Invalid_number_of_argument) (* TODO *)
        else Utils.right registers.(i)

    | My_energy ->
        Utils.right Type_integer
    | My_life ->
        Utils.right Type_integer
    | Has_nutrients ->
        Utils.right Type_boolean
    | Has_particle (e1, e2) ->
        (* TODO *)
    | Has_particles ->
    | Cell_empty e ->
    | Cell_creature e ->
    | Cell_obstacle e ->

    | Random e ->
        (match type_check_expression registers e with
        | Utils.right Type_integer ->
            Utils.right Type_integer
        | Utils.right _ ->
            Utils.left (Invalid_type_of_argument) (* TODO *)
        | Utils.left _ as o -> o)
    | Random_direction ->
        Utils.right Type_direction
    | Random_boolean ->
        Utils.right Type_boolean

    | If_then_else (e1, e2, e3) ->
    | Equal (e1, e2) ->
    | Less_than (e1, e2) ->
    | Addition (e1, e2) ->
    | Multiplication (e1, e2) ->
    | Substraction (e1, e2) ->
    | Division (e1, e2) ->
    | Modulo (e1, e2) ->
    | And (e1, e2) ->
    | Or (e1, e2) ->
    | Not e ->
    | Opposite e ->
    | Turn_left e ->
    | Turn_right e ->
    | Apply (e, l) ->

(** Type check whether a program is safe (returning None), or returns the error if any. **)
val type_check : program -> type_check_error option

(** Actions / side effects of a program **)
type action =
    | Action_wait
    | Action_move of direction
    | Action_produce_particules of int
    | Action_collect_particules
    | Action_produce_nutrient of int
    | Action_consume_nutrient
    | Action_clone
        of direction
        * int
        * program_state

(** Execute one instruction of a program **)
val execute : program -> program_state -> action option * program_state

(** Iterate the execution a given number of time. If no action is emitted when the bound is reached, the chosen action is Action_wait. **)
val execute_several : int (** Time given **) -> program -> program_state -> action * program_state

