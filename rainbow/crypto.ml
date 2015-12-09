open Cryptokit
open JoinPool.Simple


let make_hash_f h =
    if h = "sha-2/256" then
        hash_string (Hash.sha2 256)
    else if  h = "sha-2/512" then
        hash_string (Hash.sha2 512)
    else if h = "md5" then
        hash_string (Hash.md5 ())
    else if h = "sha-3/256" then
        hash_string (Hash.sha3 256)
    else if h = "sha-3/512" then
        hash_string (Hash.sha3 512)
    else if h = "ripemd-160" then
        hash_string (Hash.ripemd160 ())
    else
        failwith "unknown hash function"

let get_hash_len h =
    if h = "sha-2/256" || h = "sha-3/256" then 32
    else if  h = "sha-2/512" || h = "sha-3/512" then 64
    else if h = "md5" then 16
    else if h = "ripemd160" then 20
    else
        failwith "unknown hash function"


(* not available in ocaml 4.01... *)
let string_init n f =
    let s = String.create n in
    for i = 0 to n - 1 do
        s.[i] <- f i;
    done;
    s

(** Cree une fonction de reduction qui a un sel et un hash associe une chain
    de longueur [length] de caracteres de [charset]. *)
let make_rdx_f charset pw_len h_len =
    let n_char = String.length charset in
    let init salt h i = charset.[(int_of_char h.[i mod h_len] lxor salt) mod n_char] in
    fun salt h -> string_init pw_len (init salt h)


(** Enumeration prenant en argument une seed et renvoyant n blocs de longueur
    Size.n du flot pseudo aleatoire RC4. *)
let rc4seeds hash_size n =
    let null = String.make hash_size '\000' in
    let start () =
        let seed = Random.string Random.secure_rng hash_size in
        (Cipher.arcfour seed Cipher.Encrypt, n) in
    let step (arc, n) = match n with
    | 0 -> None
    | _ -> arc#put_string null; Some (arc#get_string, (arc, n - 1)) in

    {start = start; step = step}
