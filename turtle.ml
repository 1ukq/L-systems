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
  auto_synchronize false
;;

let close_after_event () =
  ignore (wait_next_event [Button_down ; Key_pressed]);
  close_graph ();
;;

(* Paramètres pour la fenêtre *)
let win_scale = 700;;


(* Fonction de conversion d'un angle en distances unitaire sur les deux axes x
   et y *)
let convert_angle a =
  let conv_deg_rad = 3.14 /. 180. in
  let angle_float = float_of_int a in
  let rad_a = angle_float *. conv_deg_rad in
  (-.(sin rad_a), cos rad_a)
;;

(* Fonction pour récupérer la prochaine position à partir de la position
   actuelle *)
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

(* Cette fonction dessine la liste de commandes dans la fenêtre une fois que la
   bonne échelle est fourni *)
let draw_cmd_list cmd_list fact first_pos =
  (* Crée la fenêtre et initialise les paramètres de Graphics *)
  create_window win_scale win_scale;
  clear_graph ();
  set_line_width 2;

  (* scale & marge pour que l'image soit au centre de la fenêtre *)
  let marge = 50 in
  let scale = (float_of_int (win_scale-(2*marge))) /.fact in

  (* paramètres de l'animation, sleep varie en fonction de la taille de la
     liste pour avoir une vitesse adaptée *)
  let sleep = 6./.(float_of_int (List.length cmd_list)) in

  (* initialisation de la première position et ajout de celle-ci dans la pile *)
  let centering_val = (float_of_int marge) /. scale in
  let pos = {x = first_pos.x +. centering_val; y = first_pos.y +. centering_val; a = first_pos.a} in
  let stored_pos = Stack.create () in
  Stack.push pos stored_pos;

  (* amène le curseur au bon endroit sur la fenètre *)
  let (x,y) = get_scaled_coord pos scale in
  moveto x y;

  (* lit cmd_list et dessine les instructions récursivement *)
  let rec parcours_liste cmd_list pos = match cmd_list with
    | [] -> ()

    | Line dist :: q ->
      let npos = get_next_pos pos dist 0 in
      let (scaled_x, scaled_y) = get_scaled_coord npos scale in
      lineto scaled_x scaled_y;
      (* animation : uniquement lorsqu'un dessin est fait *)
      synchronize ();
      Unix.sleepf(sleep);

      parcours_liste q npos

    | Move dist :: q ->
      let npos = get_next_pos pos dist 0 in
      let (scaled_x, scaled_y) = get_scaled_coord npos scale in
      moveto scaled_x scaled_y;
      parcours_liste q npos

    | Turn angl :: q ->
      let npos = get_next_pos pos 0 angl in
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

(*Cette fonction permet de récupérer les 4 extremums de la figure dessinée sur
  un plan en deux dimensions pour faciliter la mise en page de la fonction dans
  la fenêtre du dessin*)
let get_extremum cmd_list =
  (* position d'origine *)
  let pos = {x = 0.; y = 0.; a = 0} in
  let stored_pos = Stack.create () in
  Stack.push pos stored_pos;

  (* initialisation des variables pour les extremums *)
  let l = ref pos.x in
  let b = ref pos.y in
  let r = ref pos.x in
  let t = ref pos.y in

  (* lit cmd_list et cherche les extremums *)
  let rec parcours_liste cmd_list pos = match cmd_list with
    | [] -> (!l,!r,!b,!t)

    | Line dist :: q ->
      (* Recherche des extremums: On ne s'interesse qu'aux points dessinés car
         ce sont ceux qu'on veut voir dans la fenêtre *)
      (if pos.x < !l then
         l := pos.x
       else if pos.y < !b then
         b := pos.y
       else if pos.x > !r then
         r := pos.x
       else if pos.y > !t then
         t := pos.y);
      (* On continue l'algorithme normalement *)
      let npos = get_next_pos pos dist 0 in
      parcours_liste q npos

    (* Cette sous-partie n'a pas grand intérêt, elle nous permet s'implement
       d'accéder aux positions des prochains points *)
    | Move dist :: q ->
      let npos = get_next_pos pos dist 0 in
      parcours_liste q npos

    | Turn angl :: q ->
      let npos = get_next_pos pos 0 angl in
      parcours_liste q npos

    | Store :: q ->
      Stack.push pos stored_pos;
      parcours_liste q pos

    | Restore :: q ->
      let npos = Stack.pop stored_pos in
      parcours_liste q npos

  in parcours_liste cmd_list pos
;;

(* Fonction principale du fichier, elle permet de dessiner la liste de
   commandes donnée en argument, rend le dessin à la bonne échelle et fait en
   sorte qu'il s'affiche dans la fenêtre *)
let show cmd_list =
  if List.length cmd_list = 0 then failwith "liste vide"
  else
    (let (l,r,b,t) = get_extremum cmd_list in
     let fact = (max (r -. l) (t -. b)) in
     (*first_pos permet jusau'ici uniquement d'avoir le dessin dans la fenêtre*)
     let first_pos = {x = -.l ; y = -.b ; a = 0} in

     (*Fonction pour dessiner la liste de commandes*)
     draw_cmd_list cmd_list fact first_pos)
;;
