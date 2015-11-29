type t = {dat: (string, string) Hashtbl.t;
          chain_len: int;
          hash_f: string -> string;
          rdx_f: int -> string -> string;
          hash_id: string;
          rdx_id: string * int}


let create chain_len hash_id rdx_id size =
    let hash_f = Crypto.make_hash_f hash_id in
    let h_len = Crypto.get_hash_len hash_id in
    let rdx_f = Crypto.make_rdx_f rdx_id h_len in
    let dat = Hashtbl.create size in
    {dat; chain_len; hash_f; rdx_f; hash_id; rdx_id}


let dump tbl file =
    print_endline "entering `dump`";
    let chan = open_out file in
    Printf.fprintf chan "%i\000%s\000%s\000%i\000%i\000\n" tbl.chain_len tbl.hash_id
                   (fst tbl.rdx_id) (snd tbl.rdx_id) (Hashtbl.length tbl.dat);
    Hashtbl.iter (fun k v -> output_string chan (k^v)) tbl.dat;
    close_out chan


let rec read_header in_chan buf = match input_char in_chan with
| '\n' -> Buffer.contents buf
| c -> Buffer.add_char buf c; read_header in_chan buf


(** Chargement de la table. *)
let load file =
    let in_chan = open_in file in

    let header = read_header in_chan (Buffer.create 16) in
    let chain_len, hash_id, rdx_id, size =
        Scanf.sscanf header "%i\000%s@\000%s@\000%i\000%i\000" (fun a b c d e-> (a, b, (c, d), e)) in

    let hash_f = Crypto.make_hash_f hash_id in
    let h_len = Crypto.get_hash_len hash_id in
    let rdx_f = Crypto.make_rdx_f rdx_id h_len in
    let dat = Hashtbl.create size in

    for i = 0 to size - 1 do
        let last, first = String.create h_len, String.create h_len in
        really_input in_chan last 0 h_len;
        really_input in_chan first 0 h_len;
        Hashtbl.add dat last first;
    done;

    {dat; chain_len; hash_f; rdx_f; hash_id; rdx_id}


let append table file =
    let in_chan = open_in file in

    let header = read_header in_chan (Buffer.create 16) in
    let chain_len, hash_id, rdx_id, size =
        Scanf.sscanf header "%i\000%s@\000%s@\000%i\000%i\000" (fun a b c d e-> (a, b, (c, d), e)) in

    let () = match (chain_len = table.chain_len && hash_id = table.hash_id && rdx_id = table.rdx_id) with
    | true -> ()
    | false -> failwith "file not compatible with current table" in

    let h_len = Crypto.get_hash_len hash_id in

    for i = 0 to size - 1 do
        let last, first = String.create h_len, String.create h_len in
        really_input in_chan last 0 h_len;
        really_input in_chan first 0 h_len;
        Hashtbl.add table.dat last first;
    done


(** Calcule la [n]-ieme iteration de (hash o rdx i) en commencant par
    [seed] et en incrementant [i] a chaque iteration (ie calcule le
    contenu de la [i+n]-ieme colonne sachant que la colonne i contient
    [seed]). *)
let rec chain tbl seed i n = match n with
| 0 -> seed
| _ -> chain tbl (tbl.hash_f (tbl.rdx_f i seed)) (i + 1) (n - 1)


(** Calcule le dernier hash de la chaine commencant par [seed]. *)
let full_chain tbl seed = chain tbl seed 0 (tbl.chain_len - 1)


let add_chain tbl seed =
    let last = full_chain tbl seed in
    Hashtbl.add tbl.dat last seed


(** Cherche pour chaque [x] dans [seeds] si la reduction du hash de la
    [i]-ieme colonne en partant de [x] est le pw correspondant a [hash]. *)
let rec recover_aux tbl hash i seeds = match seeds with
| [] -> None
| x :: xs ->
    let pw = tbl.rdx_f i (chain tbl x 0 i) in
    if tbl.hash_f pw = hash then Some pw else recover_aux tbl hash i xs

let recover tbl hash i last =
    let seeds = Hashtbl.find_all tbl.dat last in
    recover_aux tbl hash i seeds


(** Calcule le dernier element de la chaine dont le ieme element est hash. *)
let get_last tbl hash i = chain tbl hash i (tbl.chain_len - i - 1)


(** Cherche si [hash] est dans la colonne [i] de la table (ie le pw
    recherche est la reduction du hash de la colonne [i - 1]).
    [get_first] est une fonction qui a un hash associe une liste
    (eventuellement vide) de seeds de chaines dont hash est le dernier
    element (typiquement [Hashtbl.find_all tbl]). *)
let rec crack_aux tbl hash i = match i with
| 0 -> None
| _ ->
    let last = get_last tbl hash i in
    let pw = recover tbl hash (i - 1) last in
    if pw <> None
        then pw
        else crack_aux tbl hash (i - 1)

(** Cherche  le pw correspondant a [hash]. *)
let crack tbl hash = crack_aux tbl hash (tbl.chain_len - 1)
