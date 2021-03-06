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
   déterminer une échelle adaptée pour l'affichage final dans la fenêtre; pour
   cela, on realise un processus similaire à celui utilisé lors du dessin *)
let get_extremum syst n =

  let pos = {x = 0.; y = 0.; a = 0} in

  (*pour la clareté du processus nous utilisons ici des references; on aurait
    pu mettre des variables en argument des fonctions pour éviter de les
    utiliser mais la notation paraît trop lourde*)
  let l = ref pos.x in
  let b = ref pos.y in
  let r = ref pos.x in
  let t = ref pos.y in

  let maj_extremums pos =
    if pos.x < !l then
      l := pos.x
    else if pos.y < !b then
      b := pos.y
    else if pos.x > !r then
      r := pos.x
    else if pos.y > !t then
      t := pos.y
  in

  (*cette fonction a pour but de simuler Turtle.turtle sans rien dessiner*)
  let rec turtle cmd_list pos = match cmd_list with
    | [] -> pos

    | Line dist :: q -> let npos = get_next_pos pos dist 0 in
      maj_extremums pos; maj_extremums npos;
      turtle q npos

    | Move dist :: q -> let npos = get_next_pos pos dist 0 in
      turtle q npos
    | Turn angl :: q -> turtle q (get_next_pos pos 0 angl)

    | Store :: q -> turtle q pos

    | Restore :: q -> turtle q pos
  in

  (*cette fonction facilite la transition entre show_hidden ci-dessous et
  turtle ci-dessus*)
  let rec exec syst word pos = match word with
    | Symb s -> turtle (syst.interp s) pos

    | Seq l -> let rec aux l pos = match l with
        |[] -> pos
        |t::q -> let npos = (exec syst t pos) in
          aux q npos
      in aux l pos

    | Branch w -> let npos = exec syst w pos in
      turtle [Restore] pos
  in

  (*cette fonction a pour but de simuler Systems.show en faisant appel aux
    bonnes fonctions*)
  let rec show_hidden syst word n pos =
    if n = 0 then exec syst word pos
    else (match word with
        | Symb s -> show_hidden syst (syst.rules s) (n-1) pos

        | Seq l -> (match l with
            | [] -> failwith "sequence vide"
            | [t] -> show_hidden syst t n pos
            | t::q -> let last_pos = show_hidden syst t n pos in
              show_hidden syst (Seq q) n last_pos)

        | Branch w -> let last_pos = show_hidden syst w n pos in turtle [Restore] pos)
  in
  let _ = show_hidden syst syst.axiom n pos in

  (!l,!r,!b,!t)
;;

(*Cette fonction lance le dessin à partir du bon point de départ et à la bonne
  échelle*)
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

(*Cette fonction gère la fenêtre et les commandes de l'utilisateur et lance
  le Systems.draw_syst adapté*)
let run syst =
  create_window window_scale window_scale;

  let rec launch n =
    let handle_user_cmd () =
      let event = wait_next_event [Key_pressed] in
      match event.key with
      |'e' -> close_graph ()
      |'+' -> launch (n+1)
      |'-' -> if n = 0 then launch n else launch (n-1)
      |_ -> launch n
    in
    reset_window n ;
    draw_syst syst n;
    handle_user_cmd ()
    in launch 0
;;
