(* Examples tirés du livre "The Algorithmic Beauty of Plants".
   Un exemple consiste en un axiome, un système de réécriture,
   et une interprétation. Pour plus d'exemples, voir les fichiers
   dans le répertoire examples/
*)

(* Pour l'exemple ci-dessous, ces trois symboles suffisent.
   A vous de voir ce que vous voudrez faire de ce type symbol ensuite.
*)

type symbol = A|P|M

(* snow flake  - Figure 3 du sujet *)

val snow : symbol Systems.system


type symbol_htree = A|B|C|P|M

val htree : symbol_htree Systems.system
