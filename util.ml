open Printf;;

(* Enregistrement de la table par les esclaves. A enlever de ce fichier à terme. *)


(** Auxiliaire à append. *)
let rec append_aux (chan : out_channel) (hashs : (string*string) list) =
  match hashs with
  | [] -> ()
  | x::l -> fprintf chan "%s;%s\n" (fst x) (snd x); append_aux chan l;;

(** Ajoute les couples de chaînes donnés en argument à la fin du
fichier nommé filename, avec le format "chaine1;chaine2\n". *)
let append (filename : string) (hashs : (string*string) list) =
  let chan = open_out_gen [Open_creat; Open_append] 6 filename
  in append_aux chan hashs;
  close_out chan;;

(** Enregistre une chaîne, c'est-à-dire un couple (last,seed) dans
la liste appropriee de out_array selon le découpage slices. *)
let register_chain (slices : string array) (out_array : (string*string) list array) (i : int ref) (imax : int) (seed : string) =
  if !i > imax then
    (i = ref 0;
    for k = 0 to Array.length out_array - 1 do
      append ("data"^(string_of_int k)^".txt") out_array.(k);
      out_array.(k) <- [];
    done)
  else
    let last = Util.full_chain seed
    in let morceau = Util.dicho_bornes slices last
       in out_array.(morceau) <- (last,seed)::out_array.(morceau);
          i := !i + 1;;


(* Chargement de la table. *)

let rec add_all_lines in_chan tbl =
    try
      let line = Scanf.fscanf in_chan "%s\n" (fun x -> x) in
      let i = String.index line ';' in
      let last, first = String.sub line 0 i, String.sub line (i+1) (String.length line - i - 1) in
      Hashtbl.add tbl last first;
      add_all_lines in_chan tbl;
    with
    | End_of_file -> ()

let load n file =
    let in_chan = open_in file in
    let tbl = Hashtbl.create n in
    add_all_lines in_chan tbl;
    tbl



(* Découpage de l'intervalle de hashs. *)


(** Transforme un chiffre hexadécimal en caractère. *)
let int2char (n : int) =
  if n < 10 then char_of_int (48 + n)
  else char_of_int (87 + n);;

(** Convertit un tableau de chiffres hexadécimaux en
   chaîne de caractères représentant le nombre hexadécimal. *)
let hexa_array_to_string (a : int array) =
  let s = String.make (Array.length a) '0'
  in for i=0 to (Array.length a)-1 do
    s.[i] <- int2char a.(i)
  done;
  s;;

(** Terminale, renvoie d * x**y. *)
let rec pow_term (x : int) (y : int) (d : int) =
  if y = 0 then d
  else pow_term x (y-1) (x*d);;

(** Terminale, génère la liste des chiffres
   hexadécimaux d'un entier donné, où pi et pi2 sont des variables
   évitant de recalculer les puissances de 16 à chaque itération,
   et accu un accumulateur pour rendre la fonction terminale. *)
let rec hexa_list n pi pi2 accu =
  if n = 0 then accu
  else let c = (n mod pi2) / pi
  in hexa_list (n - c*pi) pi2 (16*pi2) (c::accu);;

(** Concatène la chaîne représentant en hexadécimal h*d
   et celle constituée de q zéros. *)
let mult_hexa_2pow (h : int) (q : int) (d : int) =
  Array.append (Array.of_list (hexa_list (h*d) 1 16 []))
    (Array.make q 0);;

(** Retourne la représentation hexadécimale de la somme de
   deux nombres dont les représentations décimales sont passées en
   argument. *)
let array_hexa_sum (a1 : int array) (a2 : int array) =
  let s1, s2 = Array.length a1, Array.length a2
  in let s = max s1 s2
  in let a, r = Array.make s 0, ref 0
  in for i = 0 to (s-1) do
    let c1 = if i < s1 then a1.(s1 - i - 1) else 0
    and c2 = if i < s2 then a2.(s2 - i - 1) else 0
    in let c = (c1 + c2 + !r)
    in a.(s - i - 1) <- c mod 16; r := c / 16;
  done;
  let pref = if r <> ref 0 then [|!r|] else [| |]
  in Array.append pref a;;

(** Auxiliaire à la fonction decoupage, génère la liste des bornes
   des n1 intervalles de taille représentée par h1 et des n2
   intervalles de taille représentée par h2 *)
let rec generer_bornes (n1 : int) (n2 : int) (h1 : int array) (h2 : int array) (act : int array) =
  if n1 = 0 && n2 = 0 then [act]
  else if n1 > 0 then act :: (generer_bornes (n1-1) n2 h1 h2
  (array_hexa_sum act h1))
  else act :: (generer_bornes n1 (n2-1) h1 h2 (array_hexa_sum act h2));;


(** Auxiliaire à la fonction de découpage, choisit un entier p 
   inférieur à t et tel que 2**p soit si possible plus grand
   que ff, c'est-à-dire f*m.
   Renvoie p, 2**p, 2**p/m (division euclidienne) et 2**p mod
   m *)
let rec choix_p (t : int) (m : int) (p : int) (pp : int) (f : int) (ff : int) =
  if p == t then p, pp, pp / m, pp mod m
  else let e = (pp - ff) / m
  in if pp > ff then p, pp, f + e, pp - ff - m*e
  else choix_p t m (p+1) (2*pp) f ff;;


(** Renvoie une liste de chaînes de caractères représentant en base 16
   les bornes de m intervalles découpant l'intervalle [|0, 2**t|],
   avec une différence relative de cardinal maximale de 1/f entre deux
   intervalles. *)
let decoupage (t : int) (m : int) (f : int)=
  let p, pp, d, r = choix_p t m 0 1 f (f*m)
  in let q, h =  (t - p)/4,  pow_term 2 ((t - p) mod 4) 1
  in let h1, h2 = mult_hexa_2pow h q d, mult_hexa_2pow h q (d+1)
  in let bornes_int = Array.of_list (generer_bornes (m-r) r h1 h2
    (Array.make (Array.length h1) 0))
  in let bornes_str = Array.make (m+1) ""
  in for i = 0 to m do
    bornes_str.(i) <- hexa_array_to_string bornes_int.(i)
  done;
  bornes_str;;

(*test : decoupage 20 10 100;; *)


(** Fonction auxiliaire de dicho_bornes. Dichotomique. Prend en argument un tableau t de bornes d'intervalles d'éléments de type 'a classés par ordre lexicographique croissant et associe à l'élément s de type 'a le numéro de l'intervalle auquel il appartient, entre b1 et b2. *)
let rec dicho_bornes_aux (t : 'a array) (s : 'a) (b1 : int) (b2 : int) =
  let m = (b1 + b2) / 2
  in if b1 == m
     then b1
     else if s < t.(m)
          then dicho_bornes_aux t s b1 m
          else dicho_bornes_aux t s m b2;;

(** Dichotomique. Prend en argument un tableau t de bornes d'intervalles d'éléments de type 'a classés par ordre lexicographique croissant et associe à l'élément s de type 'a le numéro de l'intervalle auquel il appartient. *)
let dicho_bornes (t : 'a array) (s : 'a) = dicho_bornes_aux t s 0 (Array.length t - 1);;

(* test : dicho_bornes [| 0; 10; 20; 30; 40; 50; 60; 70; 80; 90; 100 |] 42;; *)
(* test : dicho_bornes [| "aaaa"; "dddd"; "llll"; "oooo"; "ssss"; "zzzz" |] "ozaml" 0 5;; *)
