open Graphics
open Turtle
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

(** Put here any type and function implementations concerning systems *)
let rec exec cmd_list pos scale = match cmd_list with
    | [] -> pos
    | Line dist :: q -> let npos = get_next_pos pos dist 0 in
      let (scaled_x, scaled_y) = get_scaled_coord npos scale in
      lineto scaled_x scaled_y;
      Unix.sleepf(0.01);
      synchronize ();
      exec q npos scale
    | Move dist :: q -> let npos = get_next_pos pos dist 0 in
      let (scaled_x, scaled_y) = get_scaled_coord npos scale in
      moveto scaled_x scaled_y;
      exec q npos scale
    | Turn angl :: q -> exec q (get_next_pos pos 0 angl) scale
    | Store :: q -> exec q pos scale
    | Restore :: q -> let (scaled_x, scaled_y) = get_scaled_coord pos scale in
      moveto scaled_x scaled_y; exec q pos scale
;;


let rec draw syst word pos scale = match word with
  | Symb s -> exec (syst.interp s) pos scale
  | Seq l -> let rec aux l pos = match l with
      |[] -> pos
      |t::q -> let npos = (draw syst t pos scale) in
        aux q npos
    in aux l pos
  | Branch w -> let npos = draw syst w pos scale in
    exec [Restore] pos scale
;;

let rec show syst word n pos scale =
  if n = 0 then draw syst word pos scale
  else (match word with
      | Symb s -> show syst (syst.rules s) (n-1) pos scale
      | Seq l -> (match l with
          | [] -> failwith "sequence vide"
          | [t] -> show syst t n pos scale
          | t::q -> let last_pos = show syst t n pos scale in show syst (Seq q) n last_pos scale)
      | Branch w -> let last_pos = show syst w n pos scale in exec [Restore] pos scale)
;;

let get_extremum syst n =

  let pos = {x = 0.; y = 0.; a = 0} in

  let l = ref pos.x in
  let b = ref pos.y in
  let r = ref pos.x in
  let t = ref pos.y in

  let rec draw_hidden syst word pos = match word with
    | Symb s -> let rec parcours_liste wl pos = match wl with
        | [] -> pos
        | Line dist :: q ->
        (if pos.x < !l then
           l := pos.x
         else if pos.y < !b then
           b := pos.y
         else if pos.x > !r then
           r := pos.x
         else if pos.y > !t then
           t := pos.y);
          let npos = get_next_pos pos dist 0 in
          (if npos.x < !l then
             l := npos.x
           else if npos.y < !b then
             b := npos.y
           else if npos.x > !r then
             r := npos.x
           else if npos.y > !t then
             t := npos.y);
          parcours_liste q npos
        | Move dist :: q -> let npos = get_next_pos pos dist 0 in
          parcours_liste q npos
        | Turn angl :: q -> parcours_liste q (get_next_pos pos 0 angl)
        | _ :: q -> parcours_liste q pos
      in parcours_liste (syst.interp s) pos
    | Seq l -> let rec aux l pos = match l with
        |[] -> pos
        |t::q -> let npos = (draw_hidden syst t pos) in
          aux q npos
      in aux l pos
    | Branch w -> pos
  in
  let rec iter syst word n pos =
    if n = 0 then  draw_hidden syst word pos
    else (match word with
        | Symb s -> iter syst (syst.rules s) (n-1) pos
        | Seq l -> (match l with
            | [] -> failwith "sequence vide"
            | [t] -> iter syst t n pos
            | t::q -> let last_pos = iter syst t n pos in iter syst (Seq q) n last_pos)
        | Branch w -> let last_pos = iter syst w n pos in pos)
  in let last_pos = iter syst syst.axiom n pos

  in (!l,!r,!b,!t)
;;


let turtle syst n =
  let (l,r,b,t) = get_extremum syst n in
  let fact = (max (r -. l) (t -. b)) in
  let scale = (float_of_int win_scale)/.fact in
  let first_pos = {x = -.l ; y = -.b ; a = 0} in

  create_window win_scale win_scale;
  clear_graph ();
  set_line_width 2;

  let (x,y) = get_scaled_coord first_pos scale in
  moveto x y;

  let last_pos = show syst syst.axiom n first_pos scale in

  synchronize ();

  close_after_event ()
;;
