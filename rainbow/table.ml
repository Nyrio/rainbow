module type TblConfig = sig
    val chain_len : int
    val hash_id : string
    val charset : string
    val pw_len : int
end


module Make (C: TblConfig) = struct
    let pw_len = C.pw_len
    let h_len = Crypto.get_h_len C.hash_id
    let hash_f = Crypto.make_hash_f C.hash_id
    let rdx_f = Crypto.make_rdx_f C.charset pw_len h_len


    type t = Hashtbl.t


    (** Create a new empty rainbow table. *)
    let create size = Hashtbl.create size


    let rec read_header_aux in_chan buf = match input_char in_chan with
    | '\n' -> Buffer.contents buf
    | c -> Buffer.add_char buf c; read_header_aux in_chan buf


    (** Load a rainbow table from file (checks configuration missmatch). *)
    let load file ?(tbl=Hashtbl.create 1000) =
        let in_chan = open_in file in
        let header = read_header in_chan (Buffer.create 16) in
        let chain_len, hash_id, charset, pw_len, size =
            Scanf.sscanf header "%i\000%s@\000%s@\000%i\000%i\000"
                         (fun a b c d e -> (a, b, c, d, e)) in
        if not (chain_len = C.chain_len && hash_id = C.hash_id &&
                charset = C.charset && pw_len = C.pw_len)
        then
            close_in in_chan;
            raise Value_error
        else
            for i = 0 to size - 1 do
                let last, first = String.create h_len, String.create h_len in
                really_input in_chan last 0 h_len;
                really_input in_chan first 0 h_len;
                Hashtbl.add tbl last first;
            done;
            close_in in_chan;
            tbl


    (** Dump a rainbow table to file. *)
    let dump tbl file =
        let chan = open_out file in
        Printf.fprintf chan "%i\000%s\000%s\000%i\000%i\000\n" C.chain_len
                       C.hash_id C.charset C.pw_len (Hashtbl.length tbl);
        Hashtbl.iter (fun k v -> output_string chan (k^v)) tbl.dat;
        close_out chan


    (** Calcule la [n]-ieme iteration de (hash o rdx i) en commencant par
        [seed] et en incrementant [i] a chaque iteration (ie calcule le contenu
        de la [i+n]-ieme colonne sachant que la colonne i contient [seed]). *)
    let rec chain seed i n = match n with
    | 0 -> seed
    | _ -> chain (hash_f (rdx_f i seed)) (i + 1) (n - 1)


    (** Calcule le dernier hash de la chaine commencant par [seed]. *)
    let full_chain seed = chain seed 0 (chain_len - 1)


    let add_chain tbl seed =
        let last = full_chain seed in
        Hashtbl.add tbl last seed


    (** Cherche pour chaque x dans [seeds] si la reduction du hash de la
        [i]-ieme colonne en partant de x est le pw correspondant a [hash]. *)
    let rec recover_aux hash i seeds = match seeds with
    | [] -> None
    | x :: xs ->
        let pw = rdx_f i (chain x 0 i) in
        if hash_f pw = hash then Some pw else recover_aux hash i xs

    let recover tbl hash i last =
        let seeds = Hashtbl.find_all tbl last in
        recover_aux hash i seeds


    (** Calcule le dernier element de la chaine dont le ieme element est hash. *)
    let get_last hash i = chain hash i (chain_len - i - 1)


    (** Cherche si [hash] est dans la colonne [i] de la table (ie le pw
        recherche est la reduction du hash de la colonne [i - 1]).  [get_first]
        est une fonction qui a un hash associe une liste (eventuellement vide)
        de seeds de chaines dont hash est le dernier element (typiquement
        [Hashtbl.find_all tbl]). *)
    let rec crack_aux tbl hash i = match i with
    | 0 -> None
    | _ ->
        let last = get_last hash i in
        let pw = recover tbl hash (i - 1) last in
        if pw <> None
            then pw
            else crack_aux tbl hash (i - 1)

    (** Cherche  le pw correspondant a [hash]. *)
    let crack tbl hash = crack_aux tbl hash (chain_len - 1)
end
