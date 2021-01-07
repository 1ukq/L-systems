open Systems
open Turtle

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

let snow : symbol system =
  let a = Symb A in
  let p = Symb P in
  let m = Symb M in
  {
    axiom = Seq [a;p;p;a;p;p;a];
    rules =
      (function
       | A -> Seq [a;m;a;p;p;a;m;a]
       | s -> Symb s);
    interp =
      (function
       | A -> [Line 30]
       | P -> [Turn 60]
       | M -> [Turn (-60)])
  }
;;

type symbol_htree = A|B|C|P|M

let htree : symbol_htree system =
  let a = Symb A in
  let b = Symb B in
  let c = Symb C in
  let p = Symb P in
  let m = Symb M in
  {
    axiom = a;
    rules =
      (function
        | A -> Seq [b; Branch (Seq [p;a]); m; a]
        | B -> Seq [c;c]
        | C -> b
        | s -> Symb s);
    interp =
      (function
        | A -> [Line 10]
        | B -> [Line 14]
        | C -> [Line 10]
        | P -> [Turn 90]
        | M -> [Turn (-90)])
  }
;;

