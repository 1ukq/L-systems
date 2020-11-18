
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

val create_window : int -> int -> unit

val close_after_event : unit -> unit

val conv_deg_rad : float

val convert_angle : int -> (float * float)

val get_next_pos : position -> int -> int -> int -> position

val get_scaled_coord : position -> int -> (int * int)

val draw : position -> (command list) -> int -> unit

val visualize : unit -> unit

val show : unit -> unit
