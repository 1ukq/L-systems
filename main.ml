open Lsystems
(** Gestion des arguments de la ligne de commande.
    Nous suggérons l'utilisation du module Arg
    http://caml.inria.fr/pub/docs/manual-ocaml/libref/Arg.html
*)

let usage = (* Entete du message d'aide pour --help *)
  "Interpretation de L-systemes et dessins fractals
   Vous pouvez utiliser la touche '+' pour monter 
   dans les itérations et '-' pour descendre.
   Utilisez la touche 'e' pour quitter."

let action_what () = Printf.printf "%s\n" usage; exit 0

let cmdline_options = [
("--what" , Arg.Unit action_what, "description");
]

let extra_arg_action = fun s -> Systems.run (Parser.build_system s)

let main () =
  Arg.parse cmdline_options extra_arg_action usage;
  print_string "Pour l'instant je ne fais rien\n"

(** On ne lance ce main que dans le cas d'un programme autonome
    (c'est-à-dire que l'on est pas dans un "toplevel" ocaml interactif).
    Sinon c'est au programmeur de lancer ce qu'il veut *)

let () = if not !Sys.interactive then main ()
