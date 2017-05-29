
(** Performs one simple and local mutation on the program. **)
val mutate : Program.program -> Program.program

(** Performs some simple but global optimisations on the program (such as constant propagation and simple dead code elimination) **)
val optimise : Program.program -> Program.program

