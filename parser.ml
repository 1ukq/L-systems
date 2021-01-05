type 's word =
  | Symb of 's
  | Seq of 's word list
  | Branch of 's word

let explode str =
    let rec explode_aux n acc =
        if(n < 0) then acc
        else explode_aux (n - 1) (str.[n] :: acc)
    in
    explode_aux (String.length str - 1) [];;

let rec concat_lists l1 l2 =
    match l1 with
        | [] -> l2
        | x :: l' -> x :: (concat_lists l' l2);;

let concat_words word1 word2 = match word1, word2 with
  |Seq l1, Seq l2 -> Seq (concat l1 l2)
  |Seq l, word2 -> Seq (concat l [word2])
  |word1, Seq l -> Seq (word1::l)
  |word1, word2 -> Seq [word1; word2];;


let axiome ax =
    let rec axiome_aux l acc =
        match l with
            | [] -> acc
            | e :: l' -> if e <> '[' && e <> ']' then axiome_aux l' (concat_words acc (Symb e)) 
                        else if e = '[' then concat_words acc (Branch (axiome_aux l' (Seq [])))
                        else acc
    in
    axiome_aux ax (Seq []);;
