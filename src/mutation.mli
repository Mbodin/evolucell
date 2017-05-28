
(** Performs one mutation on the program. **)
val mutate : Program.program -> Program.program

(** Performs some simple optimisations on the program (such as constant propagation) **)
val optimise : Program.program -> Program.program

