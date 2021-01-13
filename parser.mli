open Systems
open Turtle

val explode : string -> char list

val concat_lists : 'a list -> 'a list -> 'a list

val concat_words : 'a word -> 'a word -> 'a word

val get_lines : in_channel -> string list

val build_word : char list -> char word

val build_turtle_command : string -> command

val tuple_from_list : string list -> char word * string

val split_on_space : string -> string list

val split_all_on_space : string list -> (char word * string) list -> (char word * string) list

val rewrites_rules : (char word * string) list -> char -> char word

val interp : (char word * string) list -> char -> command list

val build_rewrites_rules : char -> string list -> char word

val build_inter : char -> string list -> command list

val build_axiom : string -> char word

val build_system : string -> char system