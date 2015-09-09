open Cryptokit


let hash_funs = Hashtbl.create 10
let () = Hashtbl.add hash_funs "sha256" (hash_string (Hash.sha256 ()), 16)
let () = Hashtbl.add hash_funs "sha512" (hash_string (Hash.sha512 ()), 32)
let () = Hashtbl.add hash_funs "md5" (hash_string (Hash.md5 ()), 8)


let make_hash = Hashtbl.find hash_funs
let make_rdx rdx_fun_id h_len =
    match rdx_fun_id.[0] with
    | '0' ->
            let (), charset, pw_len = Scanf.sscanf rdx_fun_id "%s$%s$%s"
                                                   (fun x -> ()) (fun x -> x)
                                                   (fun x -> x) in
            mk_charset_rdx charset pw_len h_len
    | _ -> failwith "unknown rdx function id"


(** Cree une fonction de reduction qui a un sel et un hash associe une chain
    de longueur [length] de caracteres de [charset]. *)
let mk_charset_rdx charset length h_length =
    let n_char = String.length charset in
    let init salt h i = charset.[(int_of_char h.[i mod h_length] lxor salt) mod n_char] in
    fun salt h -> String.init length (init salt h)


(** Enumeration prenant en argument une seed et renvoyant n blocs de longueur
    Size.n du flot pseudo aleatoire RC4. *)
let rc4seeds hash_size n =
    let null = String.make hash_size '\000' in
    {start = fun () ->
        let seed = Random.random_string Random.secure_rng hash_size in
        (Cipher.arcfour seed Cipher.encrypt, n);
     step = fun (arc, 0) -> None
        | (arc, n) -> arc#put_string null; Some (arc#get_string, (arc, n - 1))}
