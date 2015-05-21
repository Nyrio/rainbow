module type ChainConf = struct
    val chain_length: int
    val chain_number: int
    val hash_fun: string -> string
    val rdx_fun: string -> string
end

module Chain(Conf:ChainConf) = struct
    open Conf

    (** renvoie le dernier pw de la chaine [i;j[ commencant par [seed]. *)
    let rec gen_chain seed i j =
        assert ((0 <= i) && (i <= j) && (j <= chain_length));
        if i = j then seed
        else gen_chain (rdx_fun i (hash_fun seed)) (i + 1) j

    let recover_aux hash seed i =
        let pw = gen_chain seed 0 i in
        if hash_fun pw = hash then Some pw else None
    
    let rec recover hash i seeds acc = match seeds with
    | [] -> acc
    | x :: xs ->
        let pw_opt = recover_aux hash x i in
        match pw_opt with
        | None -> recover hash i xs acc
        | Some pw -> recover hash i xs (pw :: acc)

    let rec crack_aux tbl hash i =
        assert ((0 <= i) && (i < chain_length));
        let last = gen_chain (rdx_fun i hash) (i + 1) chain_length in
        
end
