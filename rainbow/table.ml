module type OneWayFct = sig
    type key
    type cipher
    val chain_len : int
    val hash_f : key -> cipher
    val rdx_f : int -> cipher -> key
end


module type S = sig
    type key
    type cipher
    val chain : cipher -> int -> int -> cipher
    val endpoint : cipher -> int -> cipher
    val recover : cipher -> int -> cipher list -> key option
    val crack : (cipher -> cipher list) -> cipher -> key option
end


module Make(F: OneWayFct) : S with type key = F.key and
                                   type cipher = F.cipher = struct
    type key = F.key
    type cipher = F.cipher

    let rec chain seed i n = match n with
    | 0 -> seed
    | _ -> chain (F.hash_f (F.rdx_f i seed)) (i + 1) (n - 1)

    let endpoint hash i = chain hash i (F.chain_len - i - 1)

    let rec recover hash i seeds = match seeds with
    | [] -> None
    | x :: xs ->
        let pw = F.rdx_f (i - 1) (chain x 0 (i-1)) in
        if F.hash_f pw = hash then Some pw else recover hash i xs

    let rec crack_aux lookup hash i = match i with
    | 0 -> None
    | _ ->
        let seeds = lookup (endpoint hash i) in
        let pw = recover hash i seeds in
        if pw <> None
            then pw
            else crack_aux lookup hash (i - 1)

    let crack lookup hash = crack_aux lookup hash (F.chain_len - 1)
end
