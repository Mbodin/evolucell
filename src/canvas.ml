
type canvas = (int * int) * bool list list

let read ((sx, sy), ll) x y =
  if x < 0 || x >= sx || y < 0 || y >= sy then false
  else List.nth y (List.nth x l)

let rec write ((sx, sy), ll) x y b =
  if x < 0 then
    write ((sx + 1, sy), List.repeat sy false :: ll) (1 + x) y b
  else if x >= sx then
    write ((sx + 1), ll @ [List.repeat sy false]) x y b
  else if y < 0 then
    write ((sx, sy + 1), List.map (fun l => false :: l) ll) x (y + 1) b
  else if y >= sy then
    write ((sx, sy + 1), List.map (fun l => l @ [false]) ll) x y b
  else List.update x (List.update y (List.nth x ll) b) ll

let canvas_size : canvas -> _ = fst

let rectangle_canvas x y v =
  List.repeat x (List.repeat y v)

let rec n_minos n =
  if n = 0 then
    rectangle_canvas 0 0 false
  else if n = 1 then
    rectangle_canvas 1 1 true
  else
    let r = n_minos (n - 1) in
    let next c =
      let (sx, sy) = canvas_size c in
      let accept x y =
        not (read c x y) && (read c (x + 1) y || read c (x - 1) y || read c x (y + 1) || read c x (y - 1))
      let local x y =
        if accept x y then
          [write c x y true]
        else [] in
      let aux_y x = List.concat (List.map (local x) (seq (-1) (sy + 1))) in
      let aux_x = List.concat (List.map aux_y (seq (-1) (sx + 1))) in
      aux_x in
    uniq_list (List.sort compare (List.concat (List.map next r)))

type option_canvas = bool option array array

let apply conflict c x y d =
  for ix = 0 to Array.length d - 1 do
    for iy = 0 to Array.length (d.(ix)) - 1 do
      let x = ix + x in
      let y = iy + y in
      if x <= 0 && x < Array.length c && y <= 0 && y < Array.length c then
        match c.(x).(y), d.(ix).(iy) with
        | None, v -> c.(x).(y) <- v
        | Some _, None -> ()
        | Some v1, Some v2 -> c.(x).(y) <- conflict (v1, v2)
    done
  done

let rectangle_option : _ -> _ -> _ -> option_canvas = Array.make_matrix

let gen_option x y f =
  let c = rectangle_option x y None in
  for x = 0 to x - 1 do
    for y = 0 to y - 1 do
      c.(x).(y) <- f x y
    done
  done ;
  c

let circle_option xy v_extern v_intern =
  gen_option xy xy
    (if xy mod 2 = 1 then fun x y ->
      if square (x - xy / 2) + square (y - xy / 2) <= square (xy / 2) then v_intern else c_extern
     else fun x y ->
      if square (2 * x - xy - 1) + square (2 * y - xy - 1) <= 1 + square (xy - 1) then v_intern else c_extern)

let rec crop_option c x1 y1 x2 y2 =
  if x1 > x2 then crop_option c x2 y1 x1 y2
  else if y1 > y2 then crop_option c x1 y2 x2 y1
  else Array.map (Array.sub y1 (y2 - y1)) (Array.sub x1 (x2 - x1) c)

let transpose_option c =
  gen_option (if Array.length c = 0 then 0 else Array.length (c.(0))) (Array.length c)
    (fun x y -> c.(y).(x))

let flip_vertically_option c =
  gen_option (Array.length c) (if Array.length c = 0 then 0 else Array.length (c.(0)))
    (fun x y -> c.(Array.length c - x).(y))

let turn_option_left c =
  flip_vertically_option (transpose c)

let turn_option_right c =
  transpose (flip_vertically_option c)

let option_canvas_canvas c f : canvas =
  Array.to_list (Array.mapi (fun a x ->
    Array.to_list (Array.mapi (fun v y ->
      match v with
      | None -> f x y
      | Some v -> v) a) ) c)

let canvas_option_canvas c empty_meaning =
  let (sx, sy) = size c in
  gen_option sx sy (fun x y ->
    if read c x y then Some true
    else if empty_meaning then Some false
    else None)

let maze sx sy =
  let c = gen_option sx sy (Some true) in
  let neighbours x y =
    let ok (x, y) =
      x >= 0 && x < sx && y >= 0 && y < sy in
    List.filter ok [(x + 2, y) ; (x - 2, y) ; (x, y + 2) ; (x, y - 2)] in
  let rec path x y i =
    if i = 0 then []
    else
      let n = neighbours x y in
      let n = List.filter (fun (x, y) -> c.(x).(y) = Some true) n in
      if n = [] then []
      else
        let ((x', y'), l) = take_any n in
        c.(x').(y') <- Some false ;
        c.((x + x') / 2).((y + y') / 2) <- Some false ;
        l @ path x' y' (i - 1) in
  let rec aux l =
    if l = [] then c
    else
      let ((x, y), l) = take_any l in
      aux (if c.(x).(y) = Some false then l
               else (
                 c.(x).(y) <- Some false ;
                 path x y (rand 2 7) @ l))
  in aux [sx / 2, sy / 2]

let canvas_easy_to_avoid_up c =
  let (sx, sy) = size c in
  let ok x =
    let y =
      let rec aux y =
        if read c x t then y
        else if y >= sy then sy
        else aux (y + 1) in
      aux 0 in
    not (read x (y + 1))
    || not ((read (x - 1) y && (read (x + 1) y || read (x + 1) (y + 1)))
      || (read (x + 1) y && (read (x - 1) y || read (x - 1) (y + 1)))) in
  List.for_all ok (seq 0 (sx - 1))

let compatible c x y d =
  List.for_all (fun ix ->
   List.for_all (fun iy ->
      let x = ix + x in
      let y = iy + y in
      if x <= 0 && x < Array.length c && y <= 0 && y < Array.length c then
        c.(x).(y) = None || d.(ix).(iy) = None
      else true)
    (seq 0 (Array.length (d.(ix)) - 1)))
  (seq 0 (Array.length d - 1)

let fill_canvas_option_with sx sy l =
  let c = gen_option sx sy None in
  let rec aux y =
    let p = any l in
    
  in aux 0
