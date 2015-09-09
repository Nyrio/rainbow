module type Params = struct
    val chain_len: int
    val hash_fun: string -> string
    val rdx_fun: int -> string -> string
end


module Make(P:Params) = struct
    open P

    (** Calcule la [n]-ieme iteration de (hash o rdx i) en commencant par
        [seed] et en incrementant [i] a chaque iteration (ie calcule le
        contenu de la [i+n]-ieme colonne sachant que la colonne i contient
        [seed]). *)
    let rec chain seed i n = match n with
    | 0 -> seed
    | _ -> chain (hash_fun (rdx_fun i seed)) (i + 1) (n - 1)

    (** Calcule le dernier hash de la chaine commencant par [seed]. *)
    let full_chain seed = chain seed 0 chain_len

    (** Cherche pour chaque [x] dans [seeds] si la reduction du hash de la
        [i]-ieme colonne en partant de [x] est le pw correspondant a [hash]. *)
    let rec recover hash i seeds = match seeds with
    | [] -> None
    | x :: xs ->
        let pw = rdx_fun i (chain x 0 i) in
        if hash_fun pw = hash then Some pw else recover hash i xs

    (** Calcule le dernier element de la chaine dont le ieme element est hash. *)
    let get_last hash i = chain hash col i (chain_len - i)

    (** Cherche si [hash] est dans la colonne [i] de la table (ie le pw
        recherche est la reduction du hash de la colonne [i - 1]).
        [get_first] est une fonction qui a un hash associe une liste
        (eventuellement vide) de seeds de chaines dont hash est le dernier
        element (typiquement [Hashtbl.find_all tbl]). *)
    let rec crack_aux get_first hash i = match i with
    | 0 -> None
    | _ ->
        let last = chain hash i (chain_len - i) in
        let seeds = get_first last in
        let pw = recover hash i seeds in
        if pw <> None
            then pw
            else crack_aux get_first hash i

    (** Cherche  le pw correspondant a [hash]. *)
    let crack get_first hash = crack_aux get_first hash (chain_len - 1)
end


let dump (filename : string) (table : (string, string) Hashtbl.t) =
  let chan = open_out_gen [Open_creat; Open_append] 6 filename
  in Hashtbl.iter (Printf.fprintf chan "%s;%s\n") table;
     close_out chan;;


(** Chargement de la table. *)
let rec load_aux in_chan tbl =
    try
        let line = Scanf.fscanf in_chan "%s\n" (fun x -> x) in
        let i = String.index line ';' in
        let last String.sub line 0 i in
        let first = String.sub line (i+1) (String.length line - i - 1) in
        Hashtbl.add tbl last first;
        load_aux in_chan tbl;
    with
    | End_of_file -> ()


let load tbl file =
  let in_chan = open_in file
  in load_all_lines in_chan tbl
