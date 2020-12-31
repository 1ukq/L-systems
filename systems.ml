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
let rec concat_words word1 word2 = match word1 with
  |Symb s ->
  |Seq l ->
  |Branch w ->
;;

let rec get_ruled_word syst word n =
  if n = 0 then word
  else
    match word with
    |Symb s -> get_ruled_word syst (syst.rules s) (n-1)
    |Seq l -> let rec parcours_liste l = match l with
        |[] -> ()
        |t::q -> concat_words (get_ruled_word syst t (n-1)) (parcours_liste q)
      in parcours_liste l
    |Branch w -> Branch (get_ruled_word syst w (n-1))
;;

let rec interp_word syst word = match word with
  |Symb s -> syst.interp s
  |Seq l -> let rec parcours_liste l res = match l with
    |[] -> res
    |t::q -> parcours_liste q ((interp_word syst t) @ res)
    in parcours_liste l []
  |Branch w -> Store :: (interp_word syst w) @ [Restore]
;;

(*
let apply_rules axiom rules = match axiom with
  |Symb s -> rules s
  |Seq l -> let rec parcours_liste l res = match l with
    |[] -> res
    |(Symb s) :: q -> parcours_liste q ((rules s)::res)
    |(Seq ll) :: q -> parcours_liste q ((parcours_liste ll []) @ res)

let get_list syst n =
  let axiom = syst.axiom in
  let rec aux axiom n res = match axiom with
    |Symb s -> let next_axiom = ((syst.rules s) :: res) in
      if n = 0 then next_axiom
      else
        aux next_axiom (n-1) []
    |Seq l -> let rec parcours_liste l = match l with
        |Symb s ->
        |Seq ll ->
        |Branch w ->
    |Branch w ->
*)



(*
let concat_seq seq1 seq2 = match seq1,seq2 with
  |Seq [],seq2 -> seq2
  |seq1, Seq [] -> seq1
  |Seq l1, Seq l2 -> Seq (l1@l2)
;;

let apply_rules seq rule =
  let rec apply_rules_aux seq new_seq = match seq with
    |Seq [] -> new_seq
    |Seq (t::q) -> apply_rules_aux (Seq q) (concat_seq (rule t) new_seq)
  in apply_rules_aux seq (Seq [])
;;

let get_seq symb_syst n =
  let rec aux seq rule n = match n with
    |0 -> seq
    |n ->  aux (apply_rules seq rule) rule (n-1)
  in aux symb_syst.axiom symb_syst.rule n
;;

let get_cmd_list symb_syst n =
  let rec interpretation seq interp cmd_list = match seq with
    |Seq [] -> cmd_list
    |Seq (t::q) -> interpretation (Seq q) interp ((interp t) :: cmd_list)
  in interpretation (get_seq symb_syst n) symb_syst.interp []
;;
*)
