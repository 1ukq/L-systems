Compte rendu du projet : Les L-Systèmes
==========

## Lancement rapide
Pour lancer le projet suivez la procédure suivante :
* allez dans le répertoire `projet`
* lancez la commande `make`
* puis `./run path` où path designe le chemin absolu ou relatif vers un fichier en `.sys` (voir ceux dans `examples`)
* laissez la magie opérer, appuyez sur `+` pour augmenter le nombre d'itérations appliquées à l'axiome, `-` pour le diminuer et `e` pour fermer la fenêtre.

## Structure des fichiers
* `turtle.ml` contient ce qui est lié à l'aspect graphique du projet.
* `systems.ml` contient ce qui touche directement aux systèmes: et permet d'appliquer les itérations (rules) sur un axiome (axiom) puis fait appelle à turtle pour représenter l'itération désirée.
* `parser.ml` contient les fonctions permettant de créer un système à partir d'un fichier en `.sys` respectant une structure similaire à ceux du répertoire `examples`.
* `main.ml` est le chef d'orchestre du projet c'est lui qui fait le lien entre l'utilisateur et le code lors du lancement.

## Fonctionnement global
Lorsque l'utilisateur lance `./run path` où path représente le chemin vers le fichier du système 'syst', le fichier est lu et traduit par les fonctions dans le `parser.ml`. Le système syst qui en résulte est donné à la fonction `run` dans `systems.ml` qui lit syst.axiom. À chaque élément de syst.axiom rencontré, on calcule les n itérations de cet élément récursivement en décrémentant n. Pour chaque élément atteint lorsque n = 0, on traduit l'élément en commande pour turtle et on fait appelle à la fonction `turtle` dans `turtle.ml` qui se charge d'exécuter la commande désirée avec le module `Graphics`.
Mise à l'échelle: pour mettre le dessin à la bonne échelle, on suit le même processus que cité ci-dessus mais sans tracer aucun traits sur la fenêtre. On cherche les quatre extremums (max et min sur l'axe des abscisses et l'axe des ordonnées) en comparant les extremums temporaires aux positions trouvées. Enfin à partir des extremums trouvés on calcule une échelle qui influencera les positions dessin du système pour que celui-ci soit bien dans la fenêtre. 

## Extras
Nous avons ajouté quelques améliorations visuelles au projet original:
* Le nombre d'itérations appliquées à l'axiome s'affiche en haut à gauche de la fenêtre graphique.
* Un dégradé de couleurs permet de rendre la représentation des systèmes plus attractive (intéressant surtout pour un grand nombre d'itérations). Les couleurs varient selon la position d'une extrémité d'une ligne.
* Il est facile de changer le nombre d'itérations d'un système à l'aide des touches `+` et `-` du clavier (`e` permet de quitter le fenêtre).

## Problèmes et propositions
* Les commandes ne sont appliquées que lorsque le système a fini d'être entièrement dessiné.
* Il aurait fallu garder en mémoire la séquence de words affichée pour passer plus rapidement à l'affichage suivant lorsque l'utilisateur appuie sur `+` (il nous aurait dans ce cas suffit d'appliquer une itération aux élément de la liste au lieu de refaire tous le processus décrit plus haut) ainsi, nous aurions pu aller vraisemblablement plus loin dans le nombre d'itérations.
* On aurait voulu ajouter une animation lors du dessin mais pour de 'grandes' itérations, l'animation devenait inintéressante car déjà faite en partie par le module `Graphics` qui a du mal à tout afficher instantanément.
* Il aurait été intéressant d'ajouter la possibilité pour l'utilisateur de placer des 'miroirs' qui joueraient sur la symétrie de l'affichage par rapport à une droite placée par l'utilisateur. Pour cela il aurait 'suffit' d'appliquer les mêmes commandes aux symétriques des points par rapport à cette droite lors d'un `lineto`.
