
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

(** Describes the reaction of each particle: if any, the reaction is of the form Some (catalysor, result). **)
let data_react = Array.make number None

(** Accessors to the different aspect arrays. **)
let passive_damage i = data_passive_damage.(i)
let active_damage i = data_active_damage.(i)
let passive_heal i = data_passive_heal.(i)
let active_heal i = data_active_heal.(i)
let passive_mutate i = data_passive_mutate.(i)
let active_mutate i = data_active_mutate.(i)
let light_sensitive i = data_light_sensitive.(i)
let boost_nutrients i = data_boost_nutrients.(i)

(** The following code fills in the different aspect of all particles. **)

(** This functions writes the given tuple in all the array at index i, each element of the tuple representing one particle aspect. Reactions are treating separately. **)
let write_data (pd, ad, ph, ah, pm, am) ls bn i =
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
    let (pd, ad, ph, ah, pm, am) = representative i in
    let (pd', ad', ph', ah', pm', am') = representative i' in
    let average n1 n2 =
        (m * n1 + (representative_window - m) * n2) / m in
    (average pd pd', average ad ad', average ph ph', average ah ah', average pm pm', average am am')

(** Filling the arrays. **)
let _ =
    for i = 0 to number - 1 do
        write_data
            (average_representative (i / representative_length) (i mod representative_window))
            (i mod 6 = 3) (* Light sensitivity *)
            (i mod 7 = 3) (* Nutrient-boosting ability *)
    done

(** We now deal with reactions. **)
(* TODO *)

