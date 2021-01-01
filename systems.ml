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
let rec get_ruled_word syst word n =
  if n = 0 then word
  else
    match word with
    |Symb s -> get_ruled_word syst (syst.rules s) (n-1)
    |Seq l -> (match l with
        |[] -> failwith "Empty list in Seq"
        |[t] -> get_ruled_word syst t n
        |t::q ->
          Seq [get_ruled_word syst t n ; get_ruled_word syst (Seq q) n])
    |Branch w -> Branch (get_ruled_word syst w n)
;;

let rec interp_word syst word = match word with
  |Symb s -> syst.interp s
  |Seq l -> let rec parcours_liste l res = match l with
    |[] -> res
    |t::q -> parcours_liste q ((interp_word syst t) @ res)
    in parcours_liste l []
  |Branch w -> Store :: (interp_word syst w) @ [Restore]
;;

let get_cmd_list syst n =
  interp_word syst (get_ruled_word syst syst.axiom n)
;;
