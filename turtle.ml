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

(* Functions for graphics *)
let create_window w h =
  open_graph (" " ^ string_of_int w ^ "x" ^ string_of_int h);
  set_window_title "L-SYSTEMES   -   by Lucas & Luka";
  auto_synchronize false
;;

let close_after_event () =
  ignore (wait_next_event [Button_down ; Key_pressed]);
  close_graph ();
;;

(*window parameters*)
let win_scale = 600;;


(* Functions for angle conversion *)
let conv_deg_rad = 3.14 /. 180.;;

let convert_angle a =
  let angle_float = float_of_int a in
  let rad_a = angle_float *. conv_deg_rad in
  (-.(sin rad_a), cos rad_a)
;;

(* Function to get next position from current position: evolve with distance and angle position should be in [0,1] range in order to stay in the drawing window *)
let get_next_pos pos dist angl scale =
  let unit_dist = (float_of_int dist)/.(float_of_int scale) in
  let (unit_x,unit_y) = convert_angle pos.a in
  let nx = pos.x +. unit_x *. unit_dist in
  let ny = pos.y +. unit_y *. unit_dist in
  let na = pos.a + angl in
  {x = nx; y = ny; a = na}
;;

(* Function to get the scaled current position (scale from graphic window) *)
let get_scaled_coord pos scale =
  let x = (int_of_float (pos.x *. (float_of_int scale))) in
  let y = (int_of_float (pos.y *. (float_of_int scale))) in
  (x,y)
;;

(* Main function for Turtle, draw the lines on graphics from a list of command *)
let draw_cmd_list cmd_list fact first_pos =
  (*create window & init graphics parameters*)
  create_window win_scale win_scale;
  clear_graph ();
  set_line_width 2;

  (*scale to fit draw to window*)
  let scale = win_scale/fact in

  (*animation parameters*)
  let sleep = 6./.(float_of_int (List.length cmd_list)) in

  (*init first position & store position in a stack*)
  let pos = first_pos in
  let stored_pos = Stack.create () in
  Stack.push pos stored_pos;

  (*bring cursor to the correct position on the window*)
  let (x,y) = get_scaled_coord pos scale in
  moveto x y;

  (*read the command list and draw command on window*)
  let rec parcours_liste cmd_list pos =
    (*read command list recursively & draw instructions*)
    match cmd_list with
    | [] -> ()

    | Line dist :: q ->
      let npos = get_next_pos pos dist 0 win_scale in
      let (scaled_x, scaled_y) = get_scaled_coord npos scale in
      lineto scaled_x scaled_y;
      (*animation : only when drawing something*)
      synchronize ();
      Unix.sleepf(sleep);

      parcours_liste q npos

    | Move dist :: q ->
      let npos = get_next_pos pos dist 0 win_scale in
      let (scaled_x, scaled_y) = get_scaled_coord npos scale in
      moveto scaled_x scaled_y;
      parcours_liste q npos

    | Turn angl :: q ->
      let npos = get_next_pos pos 0 angl win_scale in
      parcours_liste q npos

    | Store :: q ->
      Stack.push pos stored_pos;
      parcours_liste q pos

    | Restore :: q ->
      let npos = Stack.pop stored_pos in
      let (scaled_x, scaled_y) = get_scaled_coord npos scale in
      moveto scaled_x scaled_y;
      parcours_liste q npos

  in parcours_liste cmd_list pos;

  close_after_event ()
;;


let get_extremum cmd_list =
  (*first position*)
  let pos = {x = 0.; y = 0.; a = 0} in
  let stored_pos = Stack.create () in
  Stack.push pos stored_pos;

  (*init extremums*)
  let l = ref pos.x in
  let b = ref pos.y in
  let r = ref pos.x in
  let t = ref pos.y in

  let rec parcours_liste cmd_list pos =
    (*read command list recursively & search for extremums*)
    match cmd_list with
    | [] -> (!l,!r,!b,!t)

    | Line dist :: q ->
      (if pos.x < !l then
         l := pos.x
       else if pos.y < !b then
         b := pos.y
       else if pos.x > !r then
         r := pos.x
       else if pos.y > !t then
         t := pos.y);
      let npos = get_next_pos pos dist 0 win_scale in
      parcours_liste q npos

    | Move dist :: q ->
      let npos = get_next_pos pos dist 0 win_scale in
      parcours_liste q npos

    | Turn angl :: q ->
      let npos = get_next_pos pos 0 angl win_scale in
      parcours_liste q npos

    | Store :: q ->
      Stack.push pos stored_pos;
      parcours_liste q pos

    | Restore :: q ->
      let npos = Stack.pop stored_pos in
      parcours_liste q npos

  in parcours_liste cmd_list pos
;;

let show cmd_list =
  if List.length cmd_list = 0 then failwith "liste vide"
  else
    (let (l,r,b,t) = get_extremum cmd_list in
     (*utiliser des valeurs absolues??*)
     let fact = (int_of_float (max (r -. l) (t -. b))) +1 in
     (*first pos is hard to fin bc of multiple conversions?*)
     let first_pos = {x = -.l ; y = -.b ; a = 0} in

     draw_cmd_list cmd_list fact first_pos)
;;
