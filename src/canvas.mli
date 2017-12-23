
(** A canvas that dynamically resizes itself when needed. A typical instanciation is with booleans. **)
type 'a canvas

(** Returns the current size of the canvas. **)
val canvas_size : 'a canvas -> int * int

(** Read the canvas at the given (possibly negative) positions. **)
val read : 'a canvas -> int -> int -> 'a

(** Write in the canvas. This may resize the canvas. Negative coordinates invalidate the previous coordinates (shifting them by the added coordinate). **)
val write : 'a canvas -> int -> int -> 'a -> 'a canvas

(** Maps the given canvas. **)
val canvas_map : ('a -> 'b) -> 'a canvas -> 'b canvas

(** A boolean canvas (with default false). **)
val boolean_canvas : bool canvas

(** A boolean option canvas (with default None). **)
val boolean_option_canvas : bool option canvas

(** Create a rectangle canvas with the given size and default element. **)
val rectangle_canvas : int -> int -> 'a -> 'a canvas

(** Change the default element used for out of bounds reads. **)
val change_default_canvas : 'a canvas -> 'a -> 'a canvas

(** The following canvas doesn’t resize itself, but is faster to use. **)
type 'a static_canvas

(** Reads the static canvas. The coordinates have to be in bounds. **)
val read_static : 'a static_canvas -> int -> int -> 'a

(** Writes the static canvas. The coordinates have to be in bounds. **)
val write_static : 'a static_canvas -> int -> int -> 'a -> 'a static_canvas

(** Returns the size of the static canvas. **)
val static_size : 'a static_canvas -> int * int

(** Maps the given canvas. **)
val canvas_static_map : ('a -> 'b) -> 'a static_canvas -> 'b static_canvas

(** Maps the given canvas, but getting the coordinate information. **)
val canvas_static_mapi : (int -> int -> 'a -> 'b) -> 'a static_canvas -> 'b static_canvas

(** Make the given static canvas a canvas with option values. **)
val make_static_option : 'a static_canvas -> 'a option static_canvas

(** Remove the optional values of the given canvas, calling the function to fill up the None case. **)
val concretize_unknown : (int -> int -> 'a) -> 'a option static_canvas -> 'a static_canvas

(** Applies a subcanvas into a larger one, from the given coordinates and using the given merging function. **)
val apply : ('a -> 'b -> 'a) -> 'a static_canvas -> int -> int -> 'b static_canvas -> 'a static_canvas

(** A frequent usage of the previous function, with option type (None having low priority). **)
val apply_option : ('a -> 'a -> 'a) -> 'a option static_canvas -> int -> int -> 'a option static_canvas -> 'a option static_canvas

(** Indicates whether the given second canvas can be applied in the given coordinates in the first without two non-None value being overlaid. **)
val compatible : 'a option static_canvas -> int -> int -> 'b option static_canvas -> bool

(** Crops the given static canvas to the given coordinates. These coordinates must fit the initial canvas. **)
val crop_static : 'a static_canvas -> int -> int -> int -> int -> 'a static_canvas

(** Generates a static canvas of the given size using the following initialising function. **)
val generate_static : int -> int -> (int -> int -> 'a) -> 'a static_canvas

(** Converts a static canvas to a dynamic one. **)
val static_canvas_canvas : 'a static_canvas -> 'a canvas

(** Converts a dynamic canvas to a static one. **)
val canvas_static_canvas : 'a canvas -> 'a static_canvas

(** Transpose the given static canvas. **)
val transpose_static : 'a static_canvas -> 'a static_canvas

(** Vertically flip the given static canvas. **)
val flip_vertically_static : 'a static_canvas -> 'a static_canvas

(** Horizontally flip the given static canvas. **)
val flip_horizontally_static : 'a static_canvas -> 'a static_canvas

(** Rotate the given static canvas to the left. **)
val rotate_left_static : 'a static_canvas -> 'a static_canvas

(** Rotate the given static canvas to the right. **)
val rotate_right_static : 'a static_canvas -> 'a static_canvas

(** Applies a central rotation on the given static canvas. **)
val rotate_static : 'a static_canvas -> 'a static_canvas

(** Generates a static canvas of the given size and fills it with the given initial element. **)
val rectangle_static : int -> int -> 'a -> 'a static_canvas

(** Generates a static canvas of the given size and fills it with a circle. The circle is filled inside with the first vaue and outside with the second. **)
val circle_static : int -> 'a -> 'a -> 'a static_canvas

(** Tests whether a given obstacle is easy to avoid with only one programming state whilst moving North. **)
val canvas_easy_to_avoid_north : bool canvas -> bool

(** Generates all the n-minos (dominos, triomonis, tetraminos, etc.) of the given size. Symetries are not taken into account. **)
val n_minos : int -> bool canvas

(* TODO
(** This functions takes the provided pieces and randomly fills in the given canvas with them. The only constraint is that no two non-None values are overlaid. The pieces are not turned. There still may be some unfilled space at the end. **)
val fill_static_canvas_with : 'a option static_canvas -> 'a option static_canvas list -> 'a option static_canvas
*)

(** Create a maze of the given size. **)
val maze : int -> int -> bool static_canvas

(** From an already defined static canvas and a list of starting points, fills the canvas with a maze. The first given value is the value for paths and the second for walls. **)
val mazify : 'a static_canvas -> 'a -> 'a -> (int * int) list -> 'a static_canvas
