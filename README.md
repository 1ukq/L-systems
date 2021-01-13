Projet PF5 2020 : L-systèmes
============================

## Modalités de rendu et d'évaluation

Voir [CONSIGNES.md](CONSIGNES.md)

## Prérequis à installer

Voir [INSTALL.md](../INSTALL.md)

  - ocaml évidemment
  - dune et make sont fortement conseillés
  - bibliothèque graphics si elle ne vient pas déjà avec ocaml

## Compilation et lancement

Par défaut, `make` est seulement utilisé pour abréger les commandes `dune` (voir `Makefile` pour plus de détails):

  - `make` sans argument lancera la compilation `dune` de `main.exe`,
    c'est-à-dire votre programme en code natif.

  - `make byte` compilera si besoin en bytecode, utile pour faire
    tourner votre code dans un toplevel OCaml, voir `lsystems.top`.

  - `make clean` pour effacer le répertoire provisoire `_build` 
    produit par `dune` lors de ses compilations.

Enfin pour lancer votre programme: `./run arg1 arg2 ...`

## Tests en mode interactif sous emacs

Votre programme doit avoir été compilé par `make byte`. Il faut bien sûr avoir installé `emacs`, 
ainsi qu'un mode ocaml pour `emacs`,  par exemple `tuareg-mode`.
  
  - Dans un fichier `start.ml` extérieur au répertoire du projet, par exemple
    au dessus du répertoire `projet`, recopiez le contenu de `lsystems.top`
    sans sa première directive  (`#ocaml init`). Ajoutez aux chemins d'accès
    des directives en `#directory` les préfixes nécessaires pour accéder aux
    mêmes répertoires (par exemple `projet/` si vous êtes au dessus de `projet`).

  - Dans le même répertoire, ouvrez sous `emacs` votre fichier de tests. Faites-le
    commencer par `#use "start.ml;;"`. Evaluez-simplement cette directive, ce qui
    lancera l'interpréteur : vous pourrez ensuite effectuer vos tests. 

En cas de recompilation du programme (toujours par `make byte`), il vous
faudra interrompre l'interpréteur par la directive `#quit;;` puis le relancer
en réévaluant `#use "start.ml;;"`.

  
