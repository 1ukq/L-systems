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

(* Cette fonction permet de parcourir et de dessiner les commandes implicites du
   word dans la fenetre (fait appelle à Turtle.turtle) *)
let rec exec syst word pos scale = match word with
  | Symb s -> turtle (syst.interp s) pos scale

  | Seq l -> let rec aux l pos = match l with
      |[] -> pos
      |t::q -> let npos = (exec syst t pos scale) in
        aux q npos
    in aux l pos

  | Branch w -> let npos = exec syst w pos scale in
    turtle [Restore] pos scale
;;

(* Cette fonction permet d'appliquer les iterations d'un axiome à partir de ses
   rules et fait appelle à draw pour dessiner l'iteration correcte *)
let rec show syst word n pos scale =
  if n = 0 then exec syst word pos scale
  else (match word with
      | Symb s -> show syst (syst.rules s) (n-1) pos scale

      | Seq l -> (match l with
          | [] -> failwith "sequence vide"
          | [t] -> show syst t n pos scale
          | t::q -> let last_pos = show syst t n pos scale in show syst (Seq q) n last_pos scale)

      | Branch w -> let last_pos = show syst w n pos scale in turtle [Restore] pos scale)
;;

(* Cette fonction a pour but de trouver les extremums du dessin pour en
   déterminer une échelle adaptée pour l'affichage final dans la fenêtre *)
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


let draw_syst syst n =
  (*init first position to get centered drawing*)
  let (l,r,b,t) = get_extremum syst n in
  let fact = (max (r -. l) (t -. b)) in
  let draw_scale = (float_of_int (window_scale-(2*marge))) /.fact in
  let centering_val = (float_of_int marge) /. draw_scale in

  let first_pos = {x = centering_val -. l;
                   y = centering_val -. b;
                   a = 0}
  in

  (*moving pen to first position*)
  let (x,y) = get_scaled_coord first_pos draw_scale in
  moveto x y;

  (*launching draw processus*)
  let _ = show syst syst.axiom n first_pos draw_scale in ()
;;

let run syst =
  create_window window_scale window_scale;

  let rec launch n =

    clear_graph ();
    draw_syst syst n;

    let status = wait_next_event [Key_pressed] in
    match status.key with
    |'e' -> close_graph ()
    |'+' -> launch (n+1)
    |'-' -> if n = 0 then launch n else launch (n-1)
    |_ -> ()
  in launch 0
;;
