type counter = {incr: unit Join.chan; decr: unit Join.chan; get: unit -> int}

let create_counter () =
    def state(n) & incr() = state(n + 1)
      or state(n) & decr() = state(n - 1)
      or state(n) & get() = state(n) & reply n to get in
    spawn state(0);
    {incr; decr; get}


type 'a async_array = {get: int -> 'a; put: (int * 'a) Join.chan}

let create_async_array size init =
    def state(a) & get(i) = state(a) & reply a.(i) to get
      or state(a) & put(i, x) = a.(i) <- x; state(a) in
    spawn state(Array.create size init);
    {get; put}


(* Découpage de l'intervalle de hashs. *)

(** Transforme un chiffre hexadécimal en caractère. *)
let hex_of_int (n : int) =
  if n < 10 then char_of_int (48 + n)
  else char_of_int (87 + n);;


(** Convertit un tableau de chiffres hexadécimaux en chaîne de caractères
    représentant le nombre hexadécimal. *)
let string_of_hex_array (a : int array) =
    let s = String.make (Array.length a) '0' in
    for i=0 to (Array.length a)-1 do
        s.[i] <- hex_of_int a.(i);
    done;
    s;;


(** Calcule [x ** y]. *)
let rec pow x y = match y with
| 0 -> 1
| 1 -> x
| _ ->
    if x mod 2 = 0 then pow (x * x) (y / 2)
    else x * (pow (x * x) ((y - 1) / 2))


(** Terminale, génère la liste des chiffres hexadécimaux d'un entier donné,
    où pi et pi2 sont des variables évitant de recalculer les puissances de 16
    à chaque itération, et accu un accumulateur pour rendre la fonction
    terminale. *)
let rec hex_list_of_int n pi pi2 accu =
    if n = 0 then accu
    else
        let c = (n mod pi2) / pi in
        hex_list_of_int (n - c*pi) pi2 (16*pi2) (c::accu);;


(** Renvoie le tableau de la representation hexadecimale de [h * d * 16**q] *)
let mult_hex_pow_2 (h : int) (q : int) (d : int) =
    Array.append (Array.of_list (hex_list_of_int (h * d) 1 16 [])) (Array.make q 0);;


(** Retourne la représentation hexadécimale de la somme de
    deux representation hexadecimales. *)
let sum_hex (a1 : int array) (a2 : int array) =
    let s1, s2 = Array.length a1, Array.length a2
    in let s = max s1 s2
    in let a, r = Array.make s 0, ref 0
    in for i = 0 to (s-1) do
        let c1 = if i < s1 then a1.(s1 - i - 1) else 0
        in let c2 = if i < s2 then a2.(s2 - i - 1) else 0
        in let c = (c1 + c2 + !r)
        in a.(s - i - 1) <- c mod 16; r := c / 16;
    done;
    let pref = if r <> ref 0 then [|!r|] else [| |]
    in Array.append pref a;;


(** Auxiliaire à la fonction slice, génère la liste des bornes des n1
    intervalles de taille représentée par h1 et des n2 intervalles de
    taille représentée par h2 *)
let rec gen_bounds (n1 : int) (n2 : int) (h1 : int array) (h2 : int array)
        (act : int array) =
    if n1 = 0 && n2 = 0 then
        [act]
    else if n1 > 0 then
        act :: (gen_bounds (n1-1) n2 h1 h2 (sum_hex act h1))
    else
        act :: (gen_bounds n1 (n2-1) h1 h2 (sum_hex act h2));;


(** Auxiliaire à la fonction de découpage, choisit un entier p
    inférieur à t et tel que 2**p soit si possible plus grand que ff,
    c'est-à-dire f*m. Renvoie p, 2**p, 2**p/m (division euclidienne) et [2**p
    mod m] *)
let rec choose_pow (t : int) (m : int) (p : int) (pp : int) (f : int) (ff : int) =
    if p == t then
        p, pp, pp / m, pp mod m
    else
        let e = (pp - ff) / m
        in if pp > ff then
            p, pp, f + e, pp - ff - m*e
        else
            choose_pow t m (p+1) (2*pp) f ff;;


(** Renvoie une liste de chaînes de caractères représentant en base 16
    les bornes de m intervalles découpant l'intervalle [|0, 2**t|],
    avec une différence relative de cardinal maximale de 1/f entre deux
    intervalles. *)
let slice (t : int) (m : int) (f : int)=
    let p, pp, d, r = choose_pow t m 0 1 f (f*m)
    in let q, h =  (t - p)/4,  pow 2 ((t - p) mod 4)
    in let h1, h2 = mult_hex_pow_2 h q d, mult_hex_pow_2 h q (d+1)
    in let bornes_int = Array.of_list (gen_bounds (m-r) r h1 h2
        (Array.make (Array.length h1) 0))
    in let bornes_str = Array.make (m+1) ""
    in for i = 0 to m do
        bornes_str.(i) <- string_of_hex_array bornes_int.(i)
    done;
    bornes_str;;


(** Fonction auxiliaire de bisect. Dichotomique. Prend en argument un
    tableau t de bornes d'intervalles d'éléments de type 'a classés par ordre
    lexicographique croissant et associe à l'élément s de type 'a le numéro de
    l'intervalle auquel il appartient, entre b1 et b2. *)
let rec bisect_aux (t : 'a array) (s : 'a) (b1 : int) (b2 : int) =
    let m = (b1 + b2) / 2
    in if b1 == m then
        b1
    else if s < t.(m) then
        bisect_aux t s b1 m
    else
        bisect_aux t s m b2;;

(** Dichotomique. Prend en argument un tableau t de bornes d'intervalles
    d'éléments de type 'a classés par ordre lexicographique croissant et
    associe à l'élément s de type 'a le numéro de l'intervalle auquel il
    appartient. *)
let bisect (t : 'a array) (s : 'a) =
    bisect_aux t s 0 (Array.length t - 1);;
