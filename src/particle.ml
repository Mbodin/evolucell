
(** Number of different particles in the game. **)
let number = 1000

(** The particle properties are defined by the following arrays, each tracking a specific aspect of the particle. The following lines declare these arrays, but do not fill them yet. **)
let data_passive_damage = Array.make number 0
let data_active_damage = Array.make number 0
let data_passive_heal = Array.make number 0
let data_active_heal = Array.make number 0
let data_passive_mutate = Array.make number 0
let data_active_mutate = Array.make number 0
let data_light_sensitive = Array.make number false
let data_boost_nutrients = Array.make number false

(** Describes the speed of each particle. **)
let data_speed = Array.make number (1, 1)

(** Describes the reaction of each particle: if any, the reaction is of the form Some (catalysor, result). **)
let data_reaction = Array.make number None

(** Accessors to the different aspect arrays. **)
let passive_damage i = data_passive_damage.(i)
let active_damage i = data_active_damage.(i)
let passive_heal i = data_passive_heal.(i)
let active_heal i = data_active_heal.(i)
let passive_mutate i = data_passive_mutate.(i)
let active_mutate i = data_active_mutate.(i)
let light_sensitive i = data_light_sensitive.(i)
let boost_nutrients i = data_boost_nutrients.(i)
let speed i = data_speed.(i)
let reaction i = data_reaction.(i)

let check_reaction i j =
  match reaction i with
  | None -> None
  | Some (j', r) ->
    if j = j' then Some r
    else None

(** The following code fills in the different aspect of all particles. **)

(** This functions writes the given tuple in all the array at index i, each element of the tuple representing one particle aspect. Reactions are treating separately. **)
let write_data i (pd, ad, ph, ah, pm, am) ls bn =
  data_passive_damage.(i) <- pd ;
  data_active_damage.(i) <- ad ;
  data_passive_heal.(i) <- ph ;
  data_active_heal.(i) <- ah ;
  data_passive_mutate.(i) <- pm ;
  data_active_mutate.(i) <- am ;
  data_light_sensitive.(i) <- ls ;
  data_boost_nutrients.(i) <- bn

(** The following array show 20 representative particles and their main components. The first particle is aimed to be reactive with a lot of other particles and has thus very few properties. In general, a high value for a passive property is 5, and a high value for an active property is 20. The light sensitivity and the nutrient boosting are spread accross particles. **)
let representative = [|
  (0, 0, 0, 0, 0, 0) ;
  (0, 0, 0, 0, 0, 5) ;
  (2, 0, 0, 0, 0, 1) ;
  (5, 0, 0, 0, 0, 0) ;
  (2, 10, 0, 0, 0, 1) ;
  (0, 20, 0, 0, 0, 0) ;
  (0, 10, 0, 0, 2, 1) ;
  (0, 5, 0, 0, 1, 10) ;
  (0, 0, 2, 0, 1, 15) ;
  (0, 2, 5, 0, 5, 7) ;
  (0, 15, 5, 0, 4, 6) ;
  (0, 0, 2, 5, 1, 1) ;
  (2, 0, 0, 15, 2, 0) ;
  (1, 0, 0, 20, 0, 1) ;
  (0, 0, 4, 5, 1, 0) ;
  (0, 0, 5, 0, 0, 0) ;
  (0, 0, 0, 0, 0, 20) ;
  (0, 0, 0, 0, 5, 0) ;
  (5, 0, 0, 20, 0, 0) ;
  (2, 0, 0, 0, 5, 6) |]

(** Size of the array representative. **)
let representative_length =
  Array.length representative

(** How many particle each representative represents. **)
let representative_window =
  number / representative_length

(** Produce a tuple being the average of the side representative. **)
let average_representative i m =
  let i' = (i + 1) mod representative_length in
  let (pd, ad, ph, ah, pm, am) = representative.(i) in
  let (pd', ad', ph', ah', pm', am') = representative.(i') in
  let big =
    (* This array lists a subtil difference between the average and the real value. This difference prevents the particle spectrum to be too boring and previsible. It comes into two variants: one for the large numbers (typically taken during absorption, with values up to 20) and the small ones. *)
    [| 0 ; 1 ; 2 ; 0; -1 ; 0 ; -1 ; 0; 1 ; 0 |]
  in
  let small =
    Array.map (fun i -> if i mod 2 = 0 then 0 else -1) big
  in
  let average subtilDifference n1 n2 =
    let tenTimesAverage =
        10 * (m * n1 + (representative_window - m) * n2) / representative_window in
    let v = tenTimesAverage / 10 + subtilDifference.(tenTimesAverage mod 10) in
    max 0 v
  in
  (average small pd pd',
   average big ad ad',
   average small ph ph',
   average big ah ah',
   average small pm pm',
   average big am am')

(** Filling the arrays. **)
let _ =
  for i = 0 to number - 1 do
    write_data i
      (average_representative (i / representative_window) (i mod representative_window))
      (i mod 22 = 3 || i mod 23 = 14) (* Light sensitivity. A ratio of approximately 1 out of 11. *)
      (i mod 26 = 3 || i mod 27 = 16) (* Nutrient-boosting ability. A ratio of approximately 1 out of 13. *)
  done

(** We now deal with reactions. **)
(* Each particle either reacts with another particle or is the product of a reaction. Never both: after a reaction is performed, the resulting particle is stable. As a consequence, there are 500 reactable particles and 500 products. The following function maps all the number from 0 to 499 to a particle number (not completely regularly). Some particles are catalysors and not considered in this isomorphism. *)
(* Maps each number from 0 to 499 to a particle which reacts with another. *)
let reactable_to_particle i = (* 0 <= i < 500 *)
  match i mod 5, i mod 7 with
  | 0, _ -> 2 * i + 1
  | _, 1 -> 2 * i + 1
  | 2, _ -> 2 * i
  | _, 3 -> 2 * i
  | 4, _ -> 2 * i + 1
  | _, 5 -> 2 * i + 1
  | _, 6 -> 2 * i
  | _, _ -> 2 * i + 1

(* Maps each number from 0 to 499 to a particle number, a reaction product. *)
let product_to_particle i = (* 0 <= i < 500 *)
  4 * i + 1 - reactable_to_particle i

let _ =
  (* These two arrays states which indexes are free to be in a reaction. *)
  let reactables = Array.make 500 true in
  let products = Array.make 500 true in
  let free_around t i =
    let rec search_step i step =
      let i = (500 + i) mod 500 in
      if t.(i) then i
      else search_step (i + step) step
    in
    let after = search_step i 1 in
    let before = search_step i (-1) in
    let d_after =
      if after >= i then
        after - i
      else
        500 - (i - after) in
    let d_before =
      if before <= i then
        i - before
      else
        500 - (before - i) in
    if d_after <= d_before then
      after
    else before
  in
  (* Tries to assign the given reaction. If already taken, finds the closest possible. *)
  let assign_reaction catalysor reactable product =
    let reactable = free_around reactables reactable in
    let product = free_around products product in
    reactables.(reactable) <- false ;
    products.(product) <- false ;
    data_reaction.(reactable_to_particle reactable) <-
      Some (catalysor, product_to_particle product)
  in
  (* There are various kinds of reactions. We here generate once after the other. *)
  (* The first 2 * 3 * 20 = 120 are inhibitors/promotion: they each map a particle either close or far from a representant, to one closer/farther. At each time, there are three variants of the inhibitor/promotion: two at one side of the representant, one in the other, with various jumps. The catalysors are in the ranges from 1 to 20 and from 480 to 499. *)
  for i = 1 to 20 do
    let center = 500 * i / 20 in
    let catalysor_promotion = i in
    let catalysor_inhibitor = 500 - i in
    let diff = [ -5, -1 ; 10, -4 ; -20, -12 ] in
    let diff_inverse = List.map (fun (a, b) -> (-a, -b)) diff in
    let diff_promotion =
      if i mod 2 = 0 then diff else diff_inverse in
    let diff_inhibitor =
      List.map (fun (a, b) -> (b, a))
        (if i mod 2 = 1 then diff else diff_inverse) in
    let diffs =
      List.map (fun (a, b) -> (catalysor_promotion, a, b)) diff_promotion
      @ List.map (fun (a, b) -> (catalysor_inhibitor, a, b)) diff_inhibitor in
    List.iter (fun (catalysor, a, b) ->
      assign_reaction catalysor (center + a) (center + b)) diffs
  done ;
  (* The second 19 * 19 = 361 jumps from each representant to the other one. The number 19 is chosen instead of 20 to make these progressively “miss” some contents in the particle array. The catalysors are in the ranges from 21 to 30 and from 471 to 479. *)
  for i = 1 to 19 do
    for j = 1 to 19 do
      let catalysor =
        if i <= 10 then
          20 + i
        else
          490 - i
      in
      assign_reaction catalysor (500 * j / 19) (500 * (i + j) / 19)
    done
  done ;
  (* The last 19 jumps have a catalyser which not near zero, but near a representent. This catalysor makes a distant particle to jump to its number. *)
  for i = 1 to 19 do
    let product = free_around products (500 * i / 20) in
    let reactable = (product + 250) / 500 in
    assign_reaction (product_to_particle product) reactable product
  done ;
  ()

(** We now deal with speed. **)
let _ =
  let l1 = [ (1, 1) ; (4, 1) ; (1, 2) ; (2, 1) ; (1, 3) ; (1, 1) ; (0, 1) ; (3, 1) ] in (* The value (1, 1) is considered to be the “usual” value, and is thus repeated twice. *)
  let l2 = List.rev_append l1 l1 @ (1, 1) :: l1 in (* The size of l2 is 25 *)
  let speed_array = Array.of_list l2 in
  for i = 0 to number - 1 do
    data_speed.(i) <- speed_array.(i mod 25)
  done


(** Type of a given particle released in the field. **)
type t =
  int
  * (int * int)
  * int

let cost_to_emit = 7
let gain_when_absorbed = 3

