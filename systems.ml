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

(* Cette fonction permet de construire le 's word de la n-ième itération de word
partir de la loi de syst.rules *)
let rec get_ruled_word syst word n =
  if n = 0 then word
  else
    match word with
    (* Dans ce cas là on applique directement la loi et on décrémente *)
    |Symb s -> get_ruled_word syst (syst.rules s) (n-1)
    (* Ici c'est une liste et comme on applique pas directement la loi,
     on ne décrémente pas*)
    |Seq l -> (match l with
        (* En pratique l ne devrait jamais être vide*)
        |[] -> failwith "Empty list in Seq"
        |[t] -> get_ruled_word syst t n
        |t::q ->
          Seq [get_ruled_word syst t n ; get_ruled_word syst (Seq q) n])
    (* Pour une Branch, on garde l'idée de Branch en appliquant la loi
    sur son contenu *)
    |Branch w -> Branch (get_ruled_word syst w n)
;;

(* Cette fonction permet de traduire un word en commandes pour turtle à partir
de syst.interp *)
let rec interp_word syst word = match word with
  (* Pour un Symb on réalise directement l'interprétation *)
  |Symb s -> syst.interp s
  (* Pour une séquence, il faut d'abord la parcourir *)
  |Seq l -> let rec parcours_liste l res = match l with
    |[] -> res
    |t::q -> parcours_liste q ((interp_word syst t) @ res)
    in parcours_liste l []
  (* Pour une branche, on explicite la branche en entourant l'interprétation de
  son contenu par Store et Restore *)
  |Branch w -> Store :: (interp_word syst w) @ [Restore]
;;

(* Fonction principale pour interpréter des 's system, celle-ci renvoie la liste
des commandes pour turtle à l'itération n *)
let get_cmd_list syst n =
  if n < 0 then failwith "Invalid iteration number"
  else interp_word syst (get_ruled_word syst syst.axiom n)
;;
