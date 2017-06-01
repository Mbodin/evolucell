
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
let data_speed = Array.make number 1

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
        let big =
            (* This array lists a subtil difference between the average and the real value. This difference prevents the particle spectrum to be too boring and previsible. It comes into two variants: one for the large numbers (typically taken during absorption, with values up to 20) and the small ones. *)
            [| 0 ; 1 ; 2 ; 0; -1 ; 0 ; -1 ; 0; 1 ; 0 |]
        in
        let small =
            Array.map (fun i -> if i mod 2 = 0 then 0 else -1) big
        in
        let tenTimesAverage subtilDifference =
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
        write_data
            (average_representative (i / representative_length) (i mod representative_window))
            (i mod 22 = 3 || i mod 23 = 14) (* Light sensitivity. A ratio of approximately 1 out of 11. *)
            (i mod 26 = 3 || i mod 27 = 16) (* Nutrient-boosting ability. A ratio of approximately 1 out of 13. *)
    done

(** We now deal with reactions. **)
(* TODO *)

(** We now deal with speed. **)
let _ =
    let l1 = [ 2 ; 5 ; 1 ; 3 ; 7 ; 2 ; 4 ; 6 ] in (* The value 2 is considered to be the “usual” value, and is thus repeated twice. *)
    let l2 = List.rev_append l1 l1 @ 2 :: l1 in (* The size of l2 is 25 *)
    let speed_array = Array.of_list l2 in
    for i = 0 to number - 1 do
        data_speed.(i) <- speed_array.(i mod 25)
    done

