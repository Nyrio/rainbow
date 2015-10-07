type t = {dat: (string, string) Hashtbl.t;
          chain_len: int;
          hash_fun: string -> string;
          rdx_fun: int -> string -> string;
          hash_fun_id: string;
          rdx_fun_id: string}


(*let create chain_len hash_fun_id rdx_fun_id size =
    let hash_fun, h_len = Crypto.make_hash hash_fun_id in
    let rdx_fun = Crypto.make_rdx rdx_fun_id h_len in
    let dat = Hashtbl.create size in
    {dat; chain_len; hash_fun; rdx_fun; hash_fun_id; rdx_fun_id}
*)

let dump tbl file =
    let chan = open_out_gen [Open_creat; Open_append] 6 file in
    Printf.fprintf chan "%i$%s$%s$%i\n" tbl.chain_len tbl.hash_fun_id
                   tbl.rdx_fun_id (Hashtbl.length tbl.dat);
    Hashtbl.iter (Printf.fprintf chan "%s;%s\n") tbl.dat;
    close_out chan


(** Chargement de la table. *)
let rec load_aux in_chan htbl =
    try
        let line = Scanf.fscanf in_chan "%s\n" (fun x -> x) in
        let (last, first) = Scanf.sscanf line "%s;%s" (fun x y -> (x,y)) in
        Hashtbl.add htbl last first;
        load_aux in_chan htbl;
    with
    | End_of_file -> ()


let load file =
    let in_chan = open_in file in

    let header = Scanf.fscanf in_chan "%s\n" (fun x -> x) in
    let chain_len, hash_fun_id, rdx_fun_id, size =
        Scanf.sscanf header "%i$%s$%s$%i" (fun a b c d -> (a, b, c, d)) in

    let rdx_fun = fun i x -> x(*Crypto.make_rdx rdx_fun_id*) in
    let hash_fun = fun x -> x(*Crypto.make_hash hash_fun_id*) in
    let dat = Hashtbl.create (2*size) in

    load_aux in_chan dat;

    {dat; chain_len; hash_fun; rdx_fun; hash_fun_id; rdx_fun_id}


(** Calcule la [n]-ieme iteration de (hash o rdx i) en commencant par
    [seed] et en incrementant [i] a chaque iteration (ie calcule le
    contenu de la [i+n]-ieme colonne sachant que la colonne i contient
    [seed]). *)
let rec chain tbl seed i n = match n with
| 0 -> seed
| _ -> chain tbl (tbl.hash_fun (tbl.rdx_fun i seed)) (i + 1) (n - 1)


(** Calcule le dernier hash de la chaine commencant par [seed]. *)
let full_chain tbl seed = chain tbl seed 0 tbl.chain_len


(** Cherche pour chaque [x] dans [seeds] si la reduction du hash de la
    [i]-ieme colonne en partant de [x] est le pw correspondant a [hash]. *)
let rec recover_aux tbl hash i seeds = match seeds with
| [] -> None
| x :: xs ->
    let pw = tbl.rdx_fun i (chain tbl x 0 i) in
    if tbl.hash_fun pw = hash then Some pw else recover_aux tbl hash i xs

let recover tbl hash i last =
    let seeds = Hashtbl.find_all tbl.dat last in
    recover_aux tbl hash i seeds


(** Calcule le dernier element de la chaine dont le ieme element est hash. *)
let get_last tbl hash i = chain tbl hash i (tbl.chain_len - i)


(** Cherche si [hash] est dans la colonne [i] de la table (ie le pw
    recherche est la reduction du hash de la colonne [i - 1]).
    [get_first] est une fonction qui a un hash associe une liste
    (eventuellement vide) de seeds de chaines dont hash est le dernier
    element (typiquement [Hashtbl.find_all tbl]). *)
let rec crack_aux tbl hash i = match i with
| 0 -> None
| _ ->
    let last = get_last tbl hash i in
    let pw = recover tbl hash i last in
    if pw <> None
        then pw
        else crack_aux tbl hash (i - 1)

(** Cherche  le pw correspondant a [hash]. *)
let crack tbl hash = crack_aux tbl hash tbl.chain_len
