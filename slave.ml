module type Conf = struct
    val hash_id: string
    val pw_charset: string
    val pw_len: int
    val chain_n: int
    val slices: string array
    val max_chains_in_mem: int
end

module Make(C:Master.Conf) = struct
    module TblConf = struct
        let rdx_fun = Table.make_rdx C.charset C.pw_length
        let hash_fun = Hashtbl.get Table.hash_funs C.hash_fun
        let chain_length = C.chain_length
    end
    let module MyTable = Table.Make(TblConf)

    let gen_array = Array.create (Array.length C.slices) []
    let gen_load = ref 0

    let generation_work (seed: string) =
        if !gen_load > imax then
            gen_load := 0;
            for k = 0 to Array.length gen_array - 1 do
                append (Printf.sprintf "data%i.txt" n) gen_array.(k);
                gen_array.(k) <- [];
            done;
        else ();
        let last = MyTable.full_chain seed in
        let i = Util.bisect C.slices last in
        gen_array.(i) <- (last, seed) :: gen_array.(i);
        gen_load := !gen_load + 1;;

    let search_gen_work (hash, col) =
        (hash, col, MyTable.chain hash col (chain_length - col - 1))

    let current_slice = Hashtbl.create C.max_chains_in_mem
    let load_slice n = MyTable.load current_slice (Printf.sprintf "data%i.txt" n)
end
