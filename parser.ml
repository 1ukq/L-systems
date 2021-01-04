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

let concat_words (w1 : 's word) (w2 : 's word) =
    match w1 with
        | Seq [] -> Seq [w2]
        | _ -> Seq [w1; w2];;

let axiome ax =
    let rec axiome_aux l acc =
        match l with
            | [] -> acc
            | e :: l' -> if e <> '[' && e <> ']' then axiome_aux l' (concat_words acc (Symb e)) 
                        else if e = '[' then concat_words acc (Branch (axiome_aux l' (Seq [])))
                        else acc
    in
    axiome_aux ax (Seq []);;
