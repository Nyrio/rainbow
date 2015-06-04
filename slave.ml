module type Conf = struct
    val hash_fun: string
    val charset: string
    val pw_length: int
    val chain_number: int
    val slices: string array
    val TODO
end

module Make(C:Conf) = struct
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
                append ("data"^(string_of_int k)^".txt") gen_array.(k);
                gen_array.(k) <- [];
            done;
        else ();
        let last = MyTable.full_chain seed in
        let i = Util.dicho_bornes C.slices last in
        gen_array.(i) <- (last, seed) :: gen_array.(i);
        gen_load := !gen_load + 1;;

    let search_gen_work (hash, col) =
        (hash, col, MyTable.chain hash col (chain_length - col - 1))

    let current_slice = Hashtbl.create C.
    let load_slice n = Util.load_table
end
