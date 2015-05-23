module type TblConf = struct
    val chain_length: int
    val chain_number: int
    val hash_fun: string -> string
    val rdx_fun: string -> string
end

module TblWork(Conf:TblConf) = struct
    open Conf

    (** renvoie le dernier pw de la chaine [i;j[ commencant par [seed]. *)
    let rec chain pw i j =
        if i = j then pw
        else gen_chain (rdx_fun i (hash_fun seed)) (i + 1) j

    let chain_of_seed seed =
        let first = rdx seed in
        (first, chain first 0 chain_length)

    let rec recover_aux hash i seeds acc = match seeds with
    | [] -> acc
    | x :: xs ->
        let pw = gen_chain x 0 i in
        if hash_fun pw = hash
            then recover hash i xs (pw :: acc)
            else recover hash i xs acc

    let recover hash i seeds = recover_aux hash i seeds []

    let rec crack_aux tbl hash i =
        let last = gen_chain (rdx_fun i hash) (i + 1) chain_length in
        let seeds = Hashtbl.find_all tbl last in
        let pws = recover hash i seeds in
        if pws <> [] then Some pws
        else if i = 0 then None else crack_aux tbl hash (i - 1)

    let crack tbl hash = crack_aux tbl hash (chain_length - 1)
end
