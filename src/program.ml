
let max_integer = Particle.number - 1

type type_variable = Utils.idt
type type_state_variable = Utils.idt
type state_index = int
type register_index = int

type register_type =
  | Type_integer
  | Type_direction
  | Type_boolean
  | Type_fun_state of register_state_type
  | Type_variable of Utils.idt

and register_state_type =
  | Type_state
  | Type_fun of register_type * register_state_type
  | Type_state_variable of type_state_variable

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
    of state_index
    * value list

type expression =
  | Constant_integer of int
  | Constant_direction of direction
  | Constant_boolean of bool
  | Constant_state of state_index
  | Register of register_index

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
    * state_index
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
    * state_index
    * expression array

type state =
  (string * string list) option
  * register_type array
  * instruction
  * int
  * state_index
  * expression array

type program =
  state array

type program_state =
  program
  * state_index
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
  | Invalid_argument_index
    of state_index
    * int
    * register_index
  | Invalid_number_of_argument
    of state_index
    * int
    * int
  | Invalid_type_of_argument
    of state_index
    * register_index
    * register_type
    * register_type
  | Invalid_state_index of state_index

type ('var, 't) type_map =
  'var Utils.union_find
  * (Utils.idt, 't) PMap.t

type variable_type_map = (type_variable, register_type) type_map
type variable_state_type_map = (type_state_variable, register_state_type) type_map

let add_variable x (u, m) =
  let (i, u) = Utils.insert_idt u x in
  (u, PMap.add i (Type_variable i))

let add_state_variable x (u, m) =
  let (i, u) = Utils.insert_idt u x in
  (u, PMap.add i (Type_state_variable i))

type typing_state =
  register_type array array
  * variable_type_map
  * variable_state_type_map

let rec merge_types typing_state t1 t2 =
  match t1, t2 with
  | Type_integer, Type_integer
  | Type_direction, Type_direction
  | Type_boolean, Type_boolean -> Utils.Right typing_state
  (* TODO: Update *)
  | Type_state l1, Type_state l2 ->
    if List.length l1 = List.length l2 then
      List.fold_left2 (fun r t1 t2 ->
        Utils.error_monad (fun typing_state ->
          merge_types typing_state t1 t2) r) (Utils.Right typing_state) l1 l2
    else Utils.Left (t1, t2)
  | Type_variable x1, Type_variable x2 ->
    let (s, (u, m), ums) = typing_state in
    let (i1, u) Utils.find_insert u x1 in
    let (i2, u) Utils.find_insert u x2 in
    let (i, u) = Utils.merge_idt u i1 i2 in
    (match PMap.find i1 m, PMap.find i2 m with
     | Type_variable _, Type_variable _ -> Utils.Right (s, (u, m), ums)
     | Type_variable x, t | t, Type_variable x ->
       Utils.Right (s, u, PMap.add i t m)
     | t1, t2 -> merge_types (s, (u, m), ums) t1 t2)
  | Type_variable x, t | t, Type_variable x ->
    let (s, (u, m), ums) = typing_state in
    let (i, u) Utils.find_insert u x in
    (match PMap.find i m with
     | Type_variable _ ->
       Utils.Right (s, (u, PMap.add i t m), ums)
     | t' -> merge_types (s, (u, m), ums) t t')
  | _, _ -> Utils.Left (t1, t2)

(* TODO: Update to the new type
val type_check_expression : typing_state -> state_index (** The current state index **) -> expression -> (type_check_error * (typing_state * expression * register_type) option, typing_state * register_type) Utils.plus
*)
let rec type_check_expression typing_state current = function
  | Constant_integer _ ->
    Utils.Right (Type_integer, typing_state)
  | Constant_direction _ ->
    Utils.Right (Type_direction, typing_state)
  | Constant_boolean _ ->
    Utils.Right (Type_boolean, typing_state)
  | Constant_state i ->
    Utils.Right (Type_state (Array.to_list (fst typing_state).(i)), typing_state)
  | Register i ->
    if i < 0 or i >= Array.length registers then
      Utils.Left (Invalid_argument_index (current, Array.length states.(current), i), None)
    else Utils.Right ((fst typing_state).(current).(i), typing_state)

  | My_energy
  | My_life ->
    Utils.Right (Type_integer, typing_state)
  | Has_nutrients ->
    Utils.Right (Type_boolean, typing_state)
  | Has_particle (e1, e2) ->
    let typing_state0 = typing_state in
    (* TODO: This error monad has to be changed: the expression-in-case-of-an-error needs to be updated! *)
    error_monad (type_check_expression typing_state current e1) (fun (t1, typing_state) ->
      error_monad (type_check_expression typing_state current e2) (fun (t2, typing_state) ->
        let merge =
          error_monad (merge_types typing_state t1 Type_integer) (fun typing_state ->
            merge_types typing_state t2 Type_integer) in
        match r with
        | Utils.Left (t1, t2) ->
          Utils.Left (Incompatible_types (current, t1, t2), Some (typing_state0, Random_boolean, Type_boolean))
        | Utils.Right typing_state -> Utils.Right (Type_boolean, typing_state)))
  | Has_particles ->
    Utils.Right (Type_boolean, typing_state)
  | Cell_empty e
  | Cell_creature e
  | Cell_obstacle e ->
    let typing_state0 = typing_state in
    (* TODO: This error monad has to be changed: the expression-in-case-of-an-error needs to be updated! *)
    error_monad (type_check_expression typing_state current e) (fun (t, typing_state) ->
      match merge_types typing_state t Type_direction with
      | Utils.Left (t1, t2) ->
        Utils.Left (Incompatible_types (current, t1, t2), Some (typing_state0, Random_boolean, Type_boolean))
      | Utils.Right typing_state -> Utils.Right (Type_boolean, typing_state))

  | Random e ->
    error_monad (type_check_expression typing_state current e) (fun (t, typing_state) ->
      match merge_types typing_state t Type_integer with
      | Utils.Left (t1, t2) ->
        Utils.Left (Incompatible_types (current, t1, t2), Some (typing_state0, Constant_integer 0, Type_integer))
      | Utils.Right typing_state -> Utils.Right (Type_integer, typing_state))
  | Random_direction ->
    Utils.Right (Type_direction, typing_state)
  | Random_boolean ->
    Utils.Right (Type_boolean, typing_state)

  | If_then_else (e1, e2, e3) ->
    error_monad (type_check_expression typing_state current e1) (fun (t1, typing_state) ->
      error_monad (type_check_expression typing_state current e2) (fun (t2, typing_state) ->
        error_monad (type_check_expression typing_state current e3) (fun (t3, typing_state) ->
          let merge =
            error_monad (merge_types typing_state t1 Type_boolean) (fun typing_state ->
              merge_types typing_state t2 t3) in
          match r with
          | Utils.Left (t1, t2) ->
            Utils.Left (Incompatible_types (current, t1, t2))
          | Utils.Right typing_state -> Utils.Right (t2, typing_state))))
  | Equal (e1, e2) ->
    error_monad (type_check_expression typing_state current e1) (fun (t1, typing_state) ->
      error_monad (type_check_expression typing_state current e2) (fun (t2, typing_state) ->
        match merge_types typing_state t1 t2 with
        | Utils.Left (t1, t2) ->
          Utils.Left (Incompatible_types (current, t1, t2))
        | Utils.Right typing_state -> Utils.Right (Type_boolean, typing_state)))
  | Less_than (e1, e2) ->
    error_monad (type_check_expression typing_state current e1) (fun (t1, typing_state) ->
      error_monad (type_check_expression typing_state current e2) (fun (t2, typing_state) ->
        let merge =
          error_monad (merge_types typing_state t1 Type_integer) (fun typing_state ->
            merge_types typing_state t2 Type_integer) in
        match r with
        | Utils.Left (t1, t2) ->
          Utils.Left (Incompatible_types (current, t1, t2))
        | Utils.Right typing_state -> Utils.Right (Type_boolean, typing_state)))
  | Addition (e1, e2)
  | Multiplication (e1, e2)
  | Substraction (e1, e2)
  | Division (e1, e2)
  | Modulo (e1, e2) ->
    error_monad (type_check_expression typing_state current e1) (fun (t1, typing_state) ->
      error_monad (type_check_expression typing_state current e2) (fun (t2, typing_state) ->
        let merge =
          error_monad (merge_types typing_state t1 Type_integer) (fun typing_state ->
            merge_types typing_state t2 Type_integer) in
        match r with
        | Utils.Left (t1, t2) ->
          Utils.Left (Incompatible_types (current, t1, t2))
        | Utils.Right typing_state -> Utils.Right (Type_integer, typing_state)))
  | And (e1, e2)
  | Or (e1, e2) ->
    error_monad (type_check_expression typing_state current e1) (fun (t1, typing_state) ->
      error_monad (type_check_expression typing_state current e2) (fun (t2, typing_state) ->
        let merge =
          error_monad (merge_types typing_state t1 Type_boolean) (fun typing_state ->
            merge_types typing_state t2 Type_boolean) in
        match r with
        | Utils.Left (t1, t2) ->
          Utils.Left (Incompatible_types (current, t1, t2))
        | Utils.Right typing_state -> Utils.Right (Type_boolean, typing_state)))
  | Not e ->
    error_monad (type_check_expression typing_state current e) (fun (t, typing_state) ->
      match merge_types typing_state t Type_boolean with
      | Utils.Left (t1, t2) ->
        Utils.Left (Incompatible_types (current, t1, t2))
      | Utils.Right typing_state -> Utils.Right (Type_boolean, typing_state))
  | Opposite e
  | Turn_left e
  | Turn_right e ->
    error_monad (type_check_expression typing_state current e) (fun (t, typing_state) ->
      match merge_types typing_state t Type_direction with
      | Utils.Left (t1, t2) ->
        Utils.Left (Incompatible_types (current, t1, t2))
      | Utils.Right typing_state -> Utils.Right (Type_direction, typing_state))
  | Apply (e, l) ->
    error_monad (type_check_expression typing_state current e) (fun (t, typing_state) ->
        let step = function
          | Utils.Left _ as r -> fun _ -> r
          | Utils.Right (typing_state, l) -> fun e ->
            error_monad (type_check_expression typing_state current e) (fun (t, typing_state) ->
              Utils.Right (typing_state, t :: l)) in
        match List.fold_left step (Utils.Right (typing_state, [])) l with
        | Utils.Left (t1, t2) ->
          Utils.Left (Incompatible_types (current, t1, t2))
        | Utils.Right (typing_state, l) ->
          let l = List.rev l in
          match t with
          | Type_variable _ -> TODO: (*Problem here: we don’t know how many argument are left to apply! *)
          TODO (* t is the type of e and l the list of type of l *))

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

