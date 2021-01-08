(** Words, rewrite systems, and rewriting *)
type 's word =
  | Symb of 's
  | Seq of 's word list
  | Branch of 's word

type 's rewrite_rules = 's -> 's word

type 's system = {
    axiom : 's word;
    rules : 's rewrite_rules;
    interp : 's -> Turtle.command list }

(** Put here any type and function interfaces concerning systems *)
val exec : 's system -> 's word -> Turtle.position -> float -> Turtle.position

val show : 's system -> 's word -> int -> Turtle.position -> float -> Turtle.position

val get_extremum : 's system -> int -> float*float*float*float

val draw_syst : 's system -> int -> unit
