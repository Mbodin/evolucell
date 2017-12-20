
%left SEQ

%%

program:
  | d = let_statement; p = program  { d :: p }
  | d = let_statement               { [d] }

let_statement:
  | LET; id = ID; args = slist (argument); EQ; l = instructions     {   }

instructions:
  | LPAR; l = instructions; RPAR                                            { l }
  | fl = finite_instructions; SEQ; l = instructions                         { fl @ l }
  | IF; e = expression; THEN; l1 = instructions; ELSE; l2 = instructions    {   }
  | c = call                                                                { [c] }

finite_instructions:
  | LPAR; l1 = finite_instructions; RPAR; l2 = finite_instructions                      { l1 @ l2 }
  | i = atomic_instruction; SEQ; l = finite_instructions                                { i :: l }
  | i = atomic_instruction                                                              { [i] }
  | IF; e = expression; THEN; l1 = finite_instructions; ELSE; l2 = finite_instructions  {   }

(* Une idée : pas de finite_instructions (c’est déterminé après le parsing).
 * IF … THEN instructions ELSE instructions
 * IF … THEN atomic_instruction SEQ instructions
*)

call:
  | id = ID; l = slist (expression)     { }

argument:
  | id = ID     { id }

atomic_instruction:
  | WAIT
  | MOVE
  | PRODUCE_PARTICLES
  | COLLECT_PARTICLES
  | PRODUCE_NUTRIENT
  | CONSUME_NUTRIENT
  | CLONE

expression:
  | CONSTANT_INTEGER
  | CONSTANT_DIRECTION
  | CONSTANT_BOOLEAN
  | ID
  | MY_ENERGY
  | MY_LIFE
  | HAS_NUTRIENTS
  | HAS_PARTICLE
  | HAS_PARTICLES
  | CELL_EMPTY
  | CELL_CREATURE
  | CELL_OBSTACLE
  …

slist (st):
  | v = st; l = slist (st)  { v :: l }
  |                         { [] }

%%

else_and_instructions:
  | ELSE; l = instructions   { l }

instructions_and_else:
  | l = else_and_instructions                               { ([], l) }
  | a = atomic_instruction; SEQ; l = instructions_and_else  { let (la, le) = l in (a :: la, le) }

instructions:
  | IF; e = expression; THEN; s1 = atomic_instruction; s2 = else_and_instructions       { [If (e, s1, s2)] }
  | IF; e = expression; THEN; a = atomic_instruction; SEQ; l = instructions_and_else    { let (la, le) = l in [If (e, a :: la, le)] }
  | IF; e = expression; THEN; a = atomic_instruction; SEQ; l = instructions             { If (e, [a], []) :: l }

%%

(*
TODO: Un parseur qui accepte beaucoup plus de chose, qui propose des changements dans le code pour le rendre compatible avec le langage présenté ici.
    Ces modifications peuvent être syntaxique (il manque un point-virgule), ou s’appliquer à des types (la variable suivante est de type booléen, mais elle est utilisé avec une addition: elle devrait donc être un entier, ou cette addition ne derait pas être écrite ainsi ; voici des suggestions pour rendre le code exécutable).
*)

