
(* Fonction dichotomique qui prend en argument un tableau t de bornes d'intervalles d'éléments de type 'a classés par ordre lexicographique croissant et associe à l'élément s de type 'a le numéro de l'intervalle auquel il appartient, entre b1 et b2. *)
let rec dicho_bornes (t : 'a vect) (s : 'a) (b1 : int) (b2 : int) =
  let m = (b1 + b2) / 2
  in if b1 == m
     then b1
     else if s < t.(m)
          then dicho_bornes t s b1 m
          else dicho_bornes t s m b2;;

(* dicho_bornes [| 0; 10; 20; 30; 40; 50; 60; 70; 80; 90; 100 |] 100 0 10;; *)
(* dicho_bornes [| "aaaa"; "dddd"; "llll"; "oooo"; "ssss"; "zzzz" |] "ozaml" 0 5;; *)

(* Fonction qui decoupe l'intervalle des chaines représentant des entiers hexadécimaux de 0 à 2**taille en m morceaux et renvoie les bornes de ces morceaux. *)
(* let rec decouper_bornes_hexastr (taille : int) (m : int) =
  unite = Big_int.div_big_int (Big_int.power_int_positive_int 2 taille) (Big_int.big_int_of_int m);; *)
