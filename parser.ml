open Pervasives

type 's element = 
    | Normal of 's
    | Bracket_open of 's
    | Bracket_close of 's

type 's word =
  | Symb of 's
  | Seq of 's word list
  | Branch of 's word


(*"Transforme" une chaîne de caractère en element list. Chaque élément de la liste est un caractère de str*)
(*
let explode str =
    let rec explode_aux n acc =
        if(n < 0) then acc
        else if str.[n] = '[' then explode_aux (n - 1) ((Bracket_open str.[n]) :: acc)
        else if str.[n] = ']' then explode_aux (n - 1) ((Bracket_close str.[n]) :: acc)
        else explode_aux (n - 1) ((Normal str.[n]) :: acc)
    in
    explode_aux (String.length str - 1) [];;
*)

let explode str =
    let rec explode_aux n acc =
        if(n < 0) then acc
        else explode_aux (n - 1) (str.[n] :: acc)
    in
    explode_aux (String.length str - 1) [];;
    


(*Concatène deux listes*)
let rec concat_lists l1 l2 =
    match l1 with
        | [] -> l2
        | x :: l' -> x :: (concat_lists l' l2);;


(*Concatène deux words*)
let concat_words word1 word2 = 
    match word1, word2 with
        | Seq l1, Seq l2 -> Seq (concat_lists l1 l2)
        | Seq l, word2 -> Seq (concat_lists l [word2])
        | word1, Seq l -> Seq (word1::l)
        | word1, word2 -> Seq [word1; word2];;





(*Trouve la ligne dans laquelle est contenue l'axiome*)
let rec get_line_ax ic =
    try
        let line = input_line ic in
        if line.[0] <> '#' && line.[0] <> '\n' then line
        else get_line_ax ic
    with e ->
        close_in_noerr ic;
        raise e

let get_axiome file_name = let ic = open_in file_name in get_line_ax ic;;

(*Trouve les lignes se situant entre les n1-ème et (n1 + 1)-ème lignes composées uniquement d'un retour charriot*)
let rec get_lines ic n1 n2 l =
    try
        let line = input_line ic in
        if String.length line > 0 && line.[0] <> '#' && n2 = n1 then get_lines ic n1 n2 (line :: l)
        else if String.length line = 0 && n2 = n1 then l
        else if String.length line = 0 then get_lines ic n1 (n2 + 1) l
        else get_lines ic n1 n2 l
    with e ->
        close_in_noerr ic;
        raise e

let get_rules file_name = let ic = open_in file_name in get_lines ic 1 0 [];;

let get_interp file_name = let ic = open_in file_name in get_lines ic 2 0 [];;


(*Construit un type word à partir d'une liste d'éléments 's: 's element -> 's word = <fun>*)
(*
let build_word ax =
    let rec build_word_aux l acc =
        match l with
            | [] -> acc
            | e :: l' ->
                        match e with
                            | Bracket_open e -> concat_words acc (Branch (build_word_aux l' (Seq [])))
                            | Bracket_close e -> acc
                            | Normal e -> build_word_aux l' (concat_words acc (Symb e))
    in
    build_word_aux ax (Seq []);;
*)

let build_word ax =
    let rec build_word_aux l acc =
        match l with
            | [] -> acc
            | e :: l' -> if e = '[' then concat_words acc (Branch (build_word_aux l' (Seq [])))
                         else if e = ']' then acc
                         else build_word_aux l' (concat_words acc (Symb e))
    in
    build_word_aux ax (Seq []);;

(*S'évalue en une certaine turtle command selon la chaîne str*)
let build_turtle_command str =
    let str_len = String.length str in
    let sub = String.sub str 1 (str_len -1) in
    let n = int_of_string sub in 
    if str.[0] = 'T' then Turtle.Turn n
    else if str.[0] = 'L' then Turtle.Line n
    else Turtle.Move n;;

(*Construit un 2-uplet à partir d'une liste de taille 2*)
let tuple_from_list l = 
    match l with
        | [] -> failwith "Liste vide"
        | [x] -> failwith "Liste n'a qu'un élément"
        | x :: y :: [] -> (Symb x.[0], y)
        | _ :: q -> failwith "Liste a plus de deux éléments";;


(*Sépare une string selon le caractère espace*)
let split_on_space str = String.split_on_char ' ' str;;

(*Sépare toutes les string de la liste l selon le caractère espace et construit une liste de 2-uplets*)
let rec split_all_on_space l acc =
    match l with
        | [] -> acc
        | str :: q -> split_all_on_space q ((tuple_from_list (split_on_space str)) :: acc);;


let rec rewrites_rules l s =
    match l with
        | [] -> Symb s
        | (Symb s', word) :: q -> if s = s' then build_word (explode word) else rewrites_rules q s
        | _ :: q -> failwith "Couple de la liste mal formé";;


let rec interp l s =
    match l with
        | [] -> []
        | (Symb s', c) :: q -> if s' = s then [build_turtle_command c] else interp q s
        | _ :: q -> failwith "Couple de la liste mal formé";;

   
let build_rewrites_rules ic s =
    let rsl = get_lines ic 1 0 [] in
    let rcl = split_all_on_space rsl [] in
    rewrites_rules rcl s;;

let build_inter ic s =
    let isl = get_lines ic 2 0 [] in
    let icl = split_all_on_space isl [] in
    interp icl s;;

let build_axiom ic = 
    let axiom_s = get_line_ax ic in
    let axiom_l = explode axiom_s in
    build_word axiom_l;;