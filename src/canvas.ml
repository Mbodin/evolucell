
type 'a canvas =
  (int * int (** Size **))
  * 'a (** Default element **)
  * 'a list list (** Data **)

let canvas_size ((size, _, _) : 'a canvas) = size

let read (((sx, sy), d, ll) : 'a canvas) x y =
  if x < 0 || x >= sx || y < 0 || y >= sy then d
  else List.nth (List.nth ll x) y

let rec write (((sx, sy), d, ll) : 'a canvas) x y b =
  if x < 0 then
    write ((sx + 1, sy), d, Utils.repeat sy d :: ll) (1 + x) y b
  else if x >= sx then
    write ((sx + 1, sy), d, ll @ [Utils.repeat sy d]) x y b
  else if y < 0 then
    write ((sx, sy + 1), d, List.map (fun l -> d :: l) ll) x (y + 1) b
  else if y >= sy then
    write ((sx, sy + 1), d, List.map (fun l -> l @ [d]) ll) x y b
  else ((sx, sy), d, Utils.list_update x (Utils.list_update y b (List.nth ll x)) ll)

let rectangle_canvas x y v : _ canvas =
  ((x, y), v, Utils.repeat x (Utils.repeat y v))

let boolean_canvas =
  rectangle_canvas 0 0 false

let boolean_option_canvas =
  rectangle_canvas 0 0 None

let change_default_canvas (size, _, ll) v =
  (size, v, ll)

let rec n_minos n =
  if n = 0 then
    [rectangle_canvas 0 0 false]
  else if n = 1 then
    [change_default_canvas (rectangle_canvas 1 1 true) false]
  else
    let r = n_minos (n - 1) in
    let next c =
      let (sx, sy) = canvas_size c in
      let accept x y =
        not (read c x y) &&
        (read c (x + 1) y || read c (x - 1) y || read c x (y + 1) || read c x (y - 1)) in
      let local x y =
        if accept x y then
          [write c x y true]
        else [] in
      let aux_y x = List.concat (List.map (local x) (Utils.seq_range (-1) (sy + 1))) in
      let aux_x = List.concat (List.map aux_y (Utils.seq_range (-1) (sx + 1))) in
      aux_x in
    Utils.uniq (List.concat (List.map next r))


type 'a static_canvas = 'a array array

let read_static c x y = c.(x).(y)

let write_static c x y v = c.(x).(y) <- v

let static_size c =
  (Array.length c, if Array.length c = 0 then 0 else Array.length (c.(0)))

let apply merge c x y d =
  for ix = 0 to Array.length d - 1 do
    for iy = 0 to Array.length (d.(ix)) - 1 do
      let x = ix + x in
      let y = iy + y in
      if x <= 0 && x < Array.length c && y <= 0 && y < Array.length c then
        write_static c x y (merge (c.(x).(y)) (d.(ix).(iy)))
    done
  done

let apply_option conflict =
  apply (fun v1 v2 ->
    match v1, v2 with
    | None, _ -> v2
    | Some _, None -> v1
    | Some v1, Some v2 -> Some (conflict v1 v2))

let rectangle_static : _ -> _ -> _ -> 'a static_canvas = Array.make_matrix

let generate_static x y f =
  let f00 = f 0 0 in (* The value is stored in case it is doing any side effect. *)
  let c = rectangle_static x y f00 in
  for x = 0 to x - 1 do
    for y = 0 to y - 1 do
      write_static c x y (if x = 0 && y = 0 then f00 else f x y)
    done
  done ;
  c

let circle_static xy v_extern v_intern =
  generate_static xy xy
    (if xy mod 2 = 1 then fun x y ->
      if Utils.square (x - xy / 2) + Utils.square (y - xy / 2)
         <= Utils.square (xy / 2) then v_intern else v_extern
     else fun x y ->
      if Utils.square (2 * x - xy + 1) + Utils.square (2 * y - xy + 1)
         <= 1 + Utils.square (xy - 1) then v_intern else v_extern)

let rec crop_static c x1 y1 x2 y2 =
  if x1 > x2 then crop_static c x2 y1 x1 y2
  else if y1 > y2 then crop_static c x1 y2 x2 y1
  else Array.map (fun a -> Array.sub a y1 (y2 - y1)) (Array.sub c x1 (x2 - x1))

let transpose_static c =
  let (sx, sy) = static_size c in
  generate_static sy sx
    (fun x y -> read_static c y x)

let flip_vertically_static c =
  let (sx, sy) = static_size c in
  generate_static sx sy
    (fun x y -> read_static c (sx - 1 - x) y)

let flip_horizontally_static c =
  let (sx, sy) = static_size c in
  generate_static sx sy
    (fun x y -> read_static c x (sy - 1 - y))

let rotate_left_static c =
  flip_vertically_static (transpose_static c)

let rotate_right_static c =
  transpose_static (flip_vertically_static c)

let rotate_static c =
  flip_horizontally_static (flip_vertically_static c)

let static_canvas_canvas c d : 'a canvas =
  (static_size c, d, List.map Array.to_list (Array.to_list c))

let canvas_static_canvas c : 'a static_canvas =
  let (sx, sy) = canvas_size c in
  generate_static sx sy (read c)

let canvas_map f (size, d, l) =
  (size, f d, List.map (List.map f) l)

let static_canvas_map f =
  Array.map (Array.map f)

let static_canvas_mapi f =
  Array.mapi (fun x a -> Array.mapi (fun y v -> f x y v) a)

let make_static_option c =
  static_canvas_map (fun v -> Some v) c

let concretize_unknown f =
  static_canvas_mapi (fun x y -> function
    | None -> f x y
    | Some v -> v)

let mazify c v_path v_wall =
  let (sx, sy) = static_size c in
  let neighbours x y =
    let ok (x, y) =
      x >= 0 && x < sx && y >= 0 && y < sy in
    List.filter ok [(x + 2, y) ; (x - 2, y) ; (x, y + 2) ; (x, y - 2)] in
  let rec path x y i =
    if i = 0 then []
    else
      let n = neighbours x y in
      let n = List.filter (fun (x, y) -> read_static c x y = v_wall) n in
      if n = [] then []
      else
        let ((x', y'), l) = Utils.take_any n in
        write_static c x' y' v_path ;
        write_static c ((x + x') / 2) ((y + y') / 2) v_path ;
        l @ path x' y' (i - 1) in
  let connect x y =
    let n = neighbours x y in
    let n = List.filter (fun (x, y) -> read_static c x y = v_path) n in
    if n = [] then ()
    else
      let (x', y') = Utils.select_any n in
      write_static c ((x + x') / 2) ((y + y') / 2) v_path in
  let rec aux l =
    if l = [] then c
    else
      let ((x, y), l) = Utils.take_any l in
      aux (if read_static c x y = v_path then l
           else (
             write_static c x y v_path ;
             connect x y ;
             path x y (Utils.rand 2 7) @ l))
  in aux

let maze sx sy =
  let c = generate_static sx sy (fun _ _ -> true) in
  mazify c false true [sx / 2, sy / 2]

let canvas_easy_to_avoid_north c =
  let (sx, sy) = canvas_size c in
  let ok x =
    let y =
      let rec aux y =
        if read c x y then y
        else if y >= sy then sy
        else aux (y + 1) in
      aux 0 in
    not (read c x (y + 1))
    || not ((read c (x - 1) y && (read c (x + 1) y || read c (x + 1) (y + 1)))
      || (read c (x + 1) y && (read c (x - 1) y || read c (x - 1) (y + 1)))) in
  List.for_all ok (Utils.seq sx)

let compatible c x y d =
  List.for_all (fun ix ->
   List.for_all (fun iy ->
      let x = ix + x in
      let y = iy + y in
      if x <= 0 && x < Array.length c && y <= 0 && y < Array.length c then
        read_static c x y = None || read_static d ix iy = None
      else true)
    (Utils.seq (Array.length (d.(ix)))))
  (Utils.seq (Array.length d))

(* TODO
let fill_static_canvas_with c l =
  let rec aux y =
    let p = any l in
    
  in aux 0
*)

let print_static_canvas (* For tests *) c print =
  for x = 0 to Array.length c - 1 do
    for y = 0 to Array.length (c.(x)) - 1 do
      print (read_static c x y)
    done ;
    print_newline ()
  done

let _ = (* For tests *)
  let m = maze 21 161 in
  print_static_canvas m (fun b -> print_char (if b then '#' else '.')) ;
  print_newline () ;
  let l = List.map canvas_static_canvas (n_minos 3) in
  List.iter (fun c ->
    print_static_canvas c (fun b -> print_char (if b then '#' else '.')) ;
    print_newline ()) l ;
  print_newline () ;
  let c = circle_static 20 true false in
  print_static_canvas c (fun b -> print_char (if b then '#' else '.'))


