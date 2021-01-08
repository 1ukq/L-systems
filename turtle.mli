
(** Turtle graphical commands *)
type command =
| Line of int      (** advance turtle while drawing *)
| Move of int      (** advance without drawing *)
| Turn of int      (** turn turtle by n degrees *)
| Store            (** save the current position of the turtle *)
| Restore          (** restore the last saved position not yet restored *)

(** Position and angle of the turtle *)
type position = {
  x: float;        (** position x *)
  y: float;        (** position y *)
  a: int;          (** angle of the direction *)
}

(** Put here any type and function signatures concerning turtle *)
val window_scale : int

val marge : int

val create_window : int -> int -> unit

val clear_window : unit -> unit

val convert_angle : int -> (float * float)

val get_next_pos : position -> int -> int -> position

val get_scaled_coord : position -> float -> (int * int)

val turtle : command list -> position -> float -> position
