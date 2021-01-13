open Pervasives
open Systems

(*Change une string en liste*)
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

let get_lines ic =
    let rec aux acc =
        try
            let line = input_line ic in
            if String.length line = 0 then acc
            else if line.[0] = '#' then aux acc
            else aux (line :: acc)
        with e ->
            close_in_noerr ic;
            raise e
    in aux [];;

(*Construit un word à partir d'une liste de char*)
let build_word ax =
    let rec build_word_aux l acc =
        match l with
            | [] -> acc
            | e :: l' -> if e = '[' then concat_words acc (build_word_aux l' (Seq []))
                         else if e = ']' then concat_words (Branch acc) (build_word_aux l' (Seq []))
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

   
let build_rewrites_rules s rsl =
    let rcl = split_all_on_space rsl [] in
    rewrites_rules rcl s;;

let build_inter s isl =
    let icl = split_all_on_space isl [] in
    interp icl s;;

let build_axiom axiom_s = 
    let axiom_l = explode axiom_s in
    build_word axiom_l;;

let build_system file_name =
    let ic = open_in file_name in
    let axiom_s = get_lines ic in
    let rsl = get_lines ic in
    let isl = get_lines ic in
    let ax = build_axiom (List.hd axiom_s) in
    let r s = build_rewrites_rules s rsl in
    let i s = build_inter s isl in
    {axiom = ax; rules = r; interp = i};;