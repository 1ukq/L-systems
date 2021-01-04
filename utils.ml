(** Put here any type and function implementations concerning utils *)

(* Function to concatenate two lists without having a Stack Overflow problem *)
let concat l1 l2 =
  let rec aux l1 acc = match l1 with
  |[] -> acc
  |t::q -> aux q (t::acc)
in aux l1 l2
;;
