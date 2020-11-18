open Graphics

type command =
| Line of int
| Move of int
| Turn of int
| Store
| Restore

type position = {
  x: float;      (** position x *)
  y: float;      (** position y *)
  a: int;        (** angle of the direction *)
}

(** Put here any type and function implementations concerning turtle *)

let create_window w h =
  open_graph (" " ^ string_of_int w ^ "x" ^ string_of_int h);
  auto_synchronize false
;;

let close_after_event () =
  ignore (wait_next_event [Button_down ; Key_pressed])
;;

let conv_deg_rad = 3.14 /. 180.;;

let convert_angle a =
  let angle_float = float_of_int a in
  let rad_a = angle_float *. conv_deg_rad in
  ((-1.) *. sin rad_a, cos rad_a)
;;

let get_next_pos pos dist angl scale =
  let unit_dist = (float_of_int dist)/.(float_of_int scale) in
  let (unit_x,unit_y) = convert_angle pos.a in
  let nx = pos.x +. unit_x *. unit_dist in
  let ny = pos.y +. unit_y *. unit_dist in
  let na = pos.a + angl in
  {x = nx; y = ny; a = na}
;;

let get_scaled_coord pos scale =
  let x = (int_of_float (pos.x *. (float_of_int scale))) in
  let y = (int_of_float (pos.y *. (float_of_int scale))) in
  (x,y)
;;

let draw pos cmd_list scale =
  let (x,y) = get_scaled_coord pos scale in
  moveto x y;

  let stored_pos = Stack.create () in
  Stack.push pos stored_pos;

  let rec parcours_liste cmd_list pos = match cmd_list with
  | [] -> print_string "EOF\n"
  | Line dist :: q ->
    let npos = get_next_pos pos dist 0 scale in
    let (scaled_x, scaled_y) = get_scaled_coord npos scale in
    lineto scaled_x scaled_y;
    parcours_liste q npos
  | Move dist :: q ->
    let npos = get_next_pos pos dist 0 scale in
    let (scaled_x, scaled_y) = get_scaled_coord npos scale in
    moveto scaled_x scaled_y;
    parcours_liste q npos
  | Turn angl :: q ->
    let npos = get_next_pos pos 0 angl scale in
    parcours_liste q npos
  | Store :: q ->
    Stack.push pos stored_pos;
    parcours_liste q pos
  | Restore :: q ->
    let npos = Stack.pop stored_pos in
    let (scaled_x, scaled_y) = get_scaled_coord npos scale in
    moveto scaled_x scaled_y;
    parcours_liste q npos
  in parcours_liste cmd_list pos
;;



let visualize () =
  let scale = 800 in
  create_window scale scale;
  clear_graph ();
  let f = Line 100 in
  let p = Turn 45 in
  let m = Turn (-45) in
  let s = Store in
  let r = Restore in
  draw {x = 0.5; y = 0.1; a = 0} [f;s;p;f;r;s;f;s;p;f;r;m;f;r;m;f;f;r;r] scale;
  synchronize ();
  close_after_event ()
;;
