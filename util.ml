open Cryptokit

let sha256 = hash_string (Hash.sha256 ())
let sha512 = hash_string (Hash.sha512 ())

(** Cree une fonction de reduction qui a un sel et un hash associe une chain
    de logueur [length] de caracteres de [charset]. *)
let mk_rdx (charset: string) (length: int) =
    let n_char = String.length charset in
    fun (salt: int) (h: string) ->
        let l = String.length h in
        let res = Bytes.create length in
        for i = 0 to length - 1 do
            Bytes.set res i charset.[(int_of_char h.[i mod l] lxor salt) mod n_char];
        done;
        res


module type HashSize = sig
    val n: int
end

(** Enumeration prenant en argument une seed et renvoyant n blocs de longueur
    Size.n du flot pseudo aleatoire RC4. *)
module RC4SeedEnum(S:Size) = struct
    let type t = string * int
    let type elt = string
    let type enum = transform * int

    let start (seed, n) = (Cipher.arcfour seed Cipher.encrypt, n)

    let null = String.make S.n '\000'
    let step (arcfour, n) = match n with
    | 0 -> None
    | _ ->
       let () = arcfour#put_string null in
       Some (arcfour#get_string, arcfour)
end


(* Fonction dichotomique qui prend en argument un tableau t de bornes d'intervalles d'éléments de type 'a classés par ordre lexicographique croissant et associe à l'élément s de type 'a le numéro de l'intervalle auquel il appartient, entre b1 et b2. *)
let rec dicho_bornes (t : 'a array) (s : 'a) (b1 : int) (b2 : int) =
  let m = (b1 + b2) / 2
  in if b1 == m
     then b1
     else if s < t.(m)
          then dicho_bornes t s b1 m
          else dicho_bornes t s m b2;;

(* test : dicho_bornes [| 0; 10; 20; 30; 40; 50; 60; 70; 80; 90; 100 |] 100 0 10;; *)
(* test : dicho_bornes [| "aaaa"; "dddd"; "llll"; "oooo"; "ssss"; "zzzz" |] "ozaml" 0 5;; *)
