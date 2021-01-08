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

(* Fonction utiles pour Graphics *)
let create_window w h =
  open_graph (" " ^ string_of_int w ^ "x" ^ string_of_int h);
  set_window_title "L-SYSTEMES   -   by Lucas & Luka";
  set_line_width 1;
  auto_synchronize true
;;

let close_after_event () =
  ignore (wait_next_event [Button_down ; Key_pressed]);
  close_graph ();
;;



(* Fonction de conversion d'un angle en distances unitaire sur les deux axes
   abscisse et ordonnée*)
let convert_angle a =
  let conv_deg_rad = 3.14 /. 180. in
  let angle_float = float_of_int a in
  let rad_a = angle_float *. conv_deg_rad in
  (-.(sin rad_a), cos rad_a)
;;

(* Fonction pour récupérer la prochaine position à partir de la position
   actuelle, d'une distance et d'un angle*)
let get_next_pos pos dist angl =
  let float_dist = float_of_int dist in
  let (unit_x,unit_y) = convert_angle pos.a in
  let nx = pos.x +. unit_x *. float_dist in
  let ny = pos.y +. unit_y *. float_dist in
  let na = pos.a + angl in
  {x = nx; y = ny; a = na}
;;

(* Fonction pour convertir la position actuelle en position à la bonne échelle
   pour avoir le dessin dans la fenêtre *)
let get_scaled_coord pos scale =
  let x = (int_of_float (pos.x *. scale)) in
  let y = (int_of_float (pos.y *. scale)) in
  (x,y)
;;


(* Fonction réalisant les execution de graphics adaptées pour une liste de
   commandes de la turtle à partir d'une position donnée en paramètres; la
   fonction renvoie la nouvelle position *)
let rec turtle cmd_list pos scale = match cmd_list with
    | [] -> pos
    | Line dist :: q -> let npos = get_next_pos pos dist 0 in
      let (scaled_x, scaled_y) = get_scaled_coord npos scale in
      lineto scaled_x scaled_y;
      (*partie animation*)
      Unix.sleepf(0.01);
      synchronize ();

      turtle q npos scale
    | Move dist :: q -> let npos = get_next_pos pos dist 0 in
      let (scaled_x, scaled_y) = get_scaled_coord npos scale in
      moveto scaled_x scaled_y;
      turtle q npos scale
    | Turn angl :: q -> turtle q (get_next_pos pos 0 angl) scale
    | Store :: q -> turtle q pos scale
    | Restore :: q -> let (scaled_x, scaled_y) = get_scaled_coord pos scale in
      moveto scaled_x scaled_y; turtle q pos scale
;;
