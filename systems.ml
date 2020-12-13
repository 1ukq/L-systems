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
