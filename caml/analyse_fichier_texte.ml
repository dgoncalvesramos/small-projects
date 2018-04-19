(*
 * Projet de programation fonctionnelle
 * GONCALVES RAMOS David
 * ENSIIE - FIPA 2A
*)

type word = char list ;;

(* Partie 1 - ECHAUFEMENT*)

(*
 * Question 1
 * is_a_letter: char -> bool 
 * Retourn true si c est valide sinon false
*)


let is_a_letter = fun c ->
  (Char.code c <= 90 && Char.code c >=65) (*Ascii code of A-Z*) 
  || (Char.code c <= 122 && Char.code c >= 97) (*Ascii code of a-z*)
  || (Char.code c = 45) (*Ascii code of -*)
  || (Char.code c = 39) (*Ascii code of '*)
;;

(*
 * Question 2
 * is_valid: word -> bool 
 * Retourne true si w est valide sinon false
*)

let is_valid = fun w ->
  match w with
  | [] -> false
  | _  -> List.for_all is_a_letter w
;;

(*
 * Question 3
 * to_lower: word -> word
 * Retourne une copie de w où les majuscules ont été remplacées par les minuscules
*)

let to_lower = fun w ->
  match w with 
  | [] -> w
  | _  -> List.map Char.lowercase w
;;

(* 
 * Question 4
 * trim: word -> word
 * Prend un mot w et retourne une copie où on a retiré tout caractère suivant la dernière lettre valdie
*)

let rec trim = fun w ->
  match List.rev w with
  | []   -> []
  | h::t -> if is_a_letter h
            then w 
            else trim (List.rev t)
;;

(*Partie 2 - Récupération de la liste de mots d'un fichier texte*)

(*******************Fonctions données*************************)

(* handle_file: (in_channel -> 'a) -> string -> 'a *)
let handle_file = fun f -> fun file ->
  let input = open_in file in
  let res = f input in
  let _ = close_in input in
  res
;;

(* my_input_char: in_channel -> char option *)
let get_char = fun input ->
  try  Some (input_char input)
  with End_of_file -> None
;;

(* update_acc: word -> in_channel -> word *)
let rec update_acc = fun acc -> fun input ->
  match get_char input with
  | None -> acc
  | Some c -> update_acc (c::acc) input
 ;;

(* words: char list -> word -> word list -> word list *)
let rec words = fun l -> fun w -> fun acc ->
  match l, w with
  | [], []    -> acc
  | [], _     -> w :: acc
  | c::cs, [] -> if c=' ' then words cs [] acc
                 else words cs [c] acc
  | c::cs, _  -> if c=' ' then words cs [] (w::acc)
                 else words cs (c::w) acc
;;


(* get_words: string -> word list *)
let get_words = fun file ->
  let lc  = handle_file (update_acc []) file in
  let lw  = words lc [] [] in
  let lw' = List.map (fun w -> to_lower (trim w)) lw in
  List.filter is_valid lw'
;;

(**********************************************************)

(*
 * Question 5
 * print_word: word -> unit
 * Fonction permettant d'afficher un mot
 *)

let print_word = fun w ->
  List.iter(Printf.printf "%c") w 
;;

(*
 * Question 7
 * nub: 'a list -> 'a list
 * Retourne la liste des éléments de l en supprimant les doublons
 *)

let rec nub = fun l ->
  match l with
  | [] -> []
  | h::t -> h::(nub(List.filter(fun x-> x <> h)t))
;;

(*
 * Question 9
 * count_words: string -> (int * int)
 * Retourne le nombre m de mots valide et le nombre n de mots différents
 * sur la donnée du chemin d'un fichier texte
*)

let count_words = fun file ->
  (List.length (get_words file),List.length(nub(get_words file)))
;;

(* Partie 3 - Structure de trie *)

module CharMap = Map.Make(Char) ;;
type trie = T of int * trie CharMap.t ;;

(* trie vide *)
let empty_trie = T (0 , CharMap.empty) ;;

(*
 * Question 10 
 * trie_get: word -> trie -> int
 * Retourne la valeur associé à w dans trie, si w n'est pas dans trie retourne 0
*)

let rec trie_get = fun w -> fun t ->
  match w with 
  | []   -> failwith "No entry find"
  | head::tail -> 
    (match t with T(v,m) -> 
      if CharMap.mem head m && tail = [] then let t' = CharMap.find head m in (match t' with T(v,m) -> v)
      else if CharMap.mem head m then trie_get tail (CharMap.find head m)
      else 0)
;;

(*
 * Question 11 
 * trie_incr: word -> trie -> trie
 * Renvoie un nouveau trie t' dans laquelle la valeur associée à w a été augmentée de 1
 *)

let rec trie_incr = fun w -> fun t ->
  match t with 
   | T(v,m) -> (match w with 
     | [] -> T((v+1), m)
     | x::xs -> if CharMap.mem x m then let s = CharMap.find x m in
					let s'= trie_incr xs s in
					let m'= CharMap.add x s' m in
					T(v,m')
                else let s = empty_trie in
		     let s'= trie_incr xs s in
		     let m'= CharMap.add x s' m in
		     T(v,m'))
;;

				      
(*
 * Question 12 
 * trie_words: string -> trie
 * Renvoie le trie construit à partir des mots d'un fichier sur la donnée de son chemin
 *)

(*
 * words_v2: char list -> word -> trie -> trie 
 * Optimisation de la fonction words, permet d'ajouter au trie t chaque mot lu 
 * depuis la liste de mots passées en paramètre, au lieu de produire une word list,
 * on ajouter chaque mot au trie t en vérifiant leur validité 
 *)

let rec words_v2 = fun l -> fun w -> fun t ->
    match l, w with
    | [], []    -> t
    | [], _     -> let word = to_lower (trim w) in
		   if is_valid word then trie_incr word t
		   else t
    | c::cs, [] -> if c=' ' then words_v2 cs [] t 
                   else words_v2 cs [c] t
    | c::cs, _  -> let word = to_lower (trim w) in 
		   if c=' ' then
		     if is_valid word then words_v2 cs [] (trie_incr word t)
		     else words_v2 cs [] t
		   else 
		    words_v2 cs (c::w) t


let trie_words = fun file -> 
  let lc  = handle_file (update_acc []) file in
  let lw  = empty_trie in words_v2 lc [] lw
;;

(* La solution suivante bien que plus rapide sur les fichiers moyennement gros
 * crash toujours sur les très gros fichiers 
  let trie_words = fun file ->
  let l = get_words file in
  List.fold_left (fun acc h -> trie_incr h acc) empty_trie l
;;
*)

(*
 * Question 13
 * trie_card: trie -> int
 * Retoune le nombre de noeuds dans trie avec une valeur non nulle
*)


let rec trie_card = fun t ->
  match t with
  | T(v,m)     -> if CharMap.is_empty m then if v=0 then 0 else 1
                  else
                   if v=0 then let cnt=0 in
		      CharMap.fold(fun key node cnt -> cnt + trie_card node) m cnt
		   else let cnt = 1 in
		      CharMap.fold(fun key node cnt -> cnt + trie_card node) m cnt
;;
    
(* 
 * Question 14
 * trie_sum: trie -> int
 * Retourne la somme des valeurs contenues dans les noeuds de trie
*)

let rec trie_sum = fun t ->
  match t with
  | T(v,m)     ->  if CharMap.is_empty m then v
                   else CharMap.fold(fun key node cnt -> cnt + trie_sum node) m v			
;;

(*
 * Question 15
 * count_words_v2: string -> (int * int)
 * Retourne le nombre m de mots valide et le nombre n de mots différents
 * sur la donnée du chemin d'un fichier texte
*)

let count_words_v2 = fun file ->
  (trie_sum(trie_words file),trie_card(trie_words file))
;;

(*
 * Question 16
 * bigger_word: trie -> (word * int)
 * Retourne le mot le plus long et sa taille dans le trie t
*)

let rec bigger_word = fun t ->
  match t with
  | T(v,m) -> CharMap.fold(fun key node acc ->
    let res = bigger_word node in
    let nextLen = match res with (w,l) -> l in
    let nextKey = match res with (w,l) -> w in
    match acc with (w,l) ->
      if l > nextLen then acc
      else let new_w = key::nextKey in
	   (new_w, List.length new_w)
  ) m ([],0)
;;

(* 
 * Question 16 
 * most_used_word: trie -> (word * int)
 * Retourne le mot le plus utilisé et son nombre d'occurences
*)

let rec most_used_word = fun t ->
  match t with
  | T(v,m) -> CharMap.fold(fun key node acc ->
    let res = most_used_word node in
    let nextV = match res with (w,v) -> v in
    let nextKey = match res with (w,v) -> w in
    match acc with (w,v) -> 
      if v > nextV then acc
      else (key::nextKey, nextV)
  ) m ([],v)
;;
