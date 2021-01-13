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

## Extras
Nous avons ajouté quelques améliorations visuelles au projet original:
* Le nombre d'itérations appliquées à l'axiome s'affiche en haut à gauche de la fenêtre graphique.
* Un dégradé de couleurs permet de rendre la représentation des systèmes plus attractive (intéressant surtout pour un grand nombre d'itérations). Les couleurs varient selon la position d'une extrémité d'une ligne.
* Il est facile de changer le nombre d'itérations d'un système à l'aide des touches `+` et `-` du clavier (`e` permet de quitter le fenêtre).

## Problèmes et propositions
* Un fichier en `.sys` doit finir par deux retour à la ligne pour être correctement interprété par le parser (tous les fichiers dans `examples` ont déjà été modifiés).
* On aurait voulu ajouter une animation lors du dessin mais pour de 'grandes' itérations, l'animation devenait inintéressante car déjà faite en partie par le module `Graphics` qui a du mal à tout afficher instantanément.
* Il aurait été intéressant d'ajouter la possibilité pour l'utilisateur de placer des 'miroirs' qui joueraient sur la symétrie de l'affichage par rapport à une droite placée par l'utilisateur. Pour cela il aurait 'suffit' d'appliquer les mêmes commandes aux symétriques des points par rapport à cette droite lors d'un `lineto`.
