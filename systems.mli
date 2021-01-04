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
val concat : 'a list -> 'a list -> 'a list

val concat_words : 's word -> 's word -> 's word

val get_ruled_word : 's system -> 's word -> int -> 's word

val interp_word : 's system -> 's word -> Turtle.command list

val get_cmd_list : 's system -> int -> Turtle.command list
