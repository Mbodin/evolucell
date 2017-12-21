
type language =
  | En
  | Eo

type lstring = language -> string

let make_lstring en eo = function
  | En -> en
  | Eo -> eo

type t =
  lstring (** Level name **)
  * lstring (** Level text **)
  * lstring option (** Optional initial (unparsed) program. Being unparsed enables to add comments, but also syntax error to be corrected, etc. **)
  * (unit -> Map.t) (** Initialising function **)
  * (Map.t -> bool) (** Function indicating whether the user won this level **)
  * Pretty.t (** Solution **)

type group =
  lstring
  * t list

(** A basic tutorial **)
let tutorial_beginner =
  (make_lstring "Beginner’s tutorial" "Kurso por komencantoj", [
    TODO: The first level just presents a simple map with six buttons (east, west, north, south, eat nutrients, create particles 10 0), with the instructions in each buttons ;
    TODO: The second level aims at nothing but presenting the user with a simple program (let loop = move west ; move north ; loop). The user is invited to edit it.
    TODO: The third level presents a goal: reaching a point north. The solution is a simple (let loop = move north ; loop).
    TODO: The fourth level is similar, but there are obstacles on the way. The description introduces “if”-statements without “else”. The starting program is (let loop = if isEmpty north then move north ; loop). Obstacles are numerous (it is a tower level): simple blocks, tetris blocks (except the one that may block a stateless program, like the “L” with its angle at the top), straight lines, crosses paving the plane, left and right piramids ;
    TODO: The fifth level aims at traversing long horizontal corridors to go north. This time we have to remember information, and the description presents the notion of booleans as well as “else” blocks. The starting program is (let loop goingWest = move north ; if goingWest then move west ; loop true else move east ; loop false). Generation includes larger corridors at times, as well as nice “rooms” with some “decorations” in it ;
    TODO: The sixth level aims at going out of a spiral. This time, we will have to store direction. We introduce the “turnLeft/turnRight” feature ;
    TODO: In the seventh level, we start in a maze. This time, there are food in the map. The goal is to eat half of it. The description introduces the “eatNutrients” command.
    TODO: In the height level, we start outside. There are food around us, and the goal is to divide ourselves (but we lack energy to safely start divinding right away). The level is won if a third generation appears.
  ])

let tutorial_advanced = TODO
let tutorial_communication = TODO

let sandbox =
  (make_lstring "Sandbox" "Provejo", [
    TODO: Various spaces with various (sometimes a lot!) creatures in it. The goal is just to survive in these worlds.
    Spaces are sorted by difficulties. In the first, creatures are very simple and food can be easily found. In the latter, creatures have group tactics and food is scarce.
  ])

let bonuses =
  (make_lstring "Bonuses" "Bonusoj", [
    TODO: A pacman-like level
  ])

let levels = [
    tutorial_beginner ;
    tutorial_advanced ;
    tutorial_communication ;
    sandbox ;
    bonuses
  ]

