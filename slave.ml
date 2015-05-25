module type Conf = sig
    val chain_length: int
    val chain_number: int
    val collision_prob: float
    val charset: string
    val pw_length: int
    val hash_fun: string
    val server_addr: Unix.addr_inet
    val server_port: int
    val slave_n: int
end


module Make(C:Conf) = struct
    module TblConf = struct
        let h_length, hash_fun = Hashtbl.get Table.hash_funs C.hash_fun
        let rdx_fun = Table.mk_rdx C.charset C.pw_length h_length
        let chain_length = C.chain_length
    end

    module Core = Table.Core(TblConf)

    let tbl = Hashtbl.create C.chain_number
    let buckets = Array.create C.slave_n []

    (** Return true if chain added to the table, else (if end point was
        already present) returns false. *)
    let gen_chain seed =
        let last = Core.full_chain seed in
        if not (Hashtbl.mem tbl last) || ((Random.float 1.) <= C.collision_prob) then
            Hashtbl.add seed last;
            true
        else false
end
