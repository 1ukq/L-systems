
val explode : string -> char list

val concat_lists : 's list -> 's list -> 's list

val build_word : 's word -> 's word -> 's word

val get_line_ax : in_channel -> string

val get_axiome : string -> string

val get_lines : in_channel -> int -> int -> string list

val build_word : char list -> char word

val build_turtle_command : string -> Turtle.command

val tuple_from_list : string list -> char word * string

val split_on_space : string -> (string list) list

val split_all_on_space : (string list) list -> (char word * string) list -> (char word * string) list