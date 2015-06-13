module type Conf = sig
    val hash_id: string
    val pw_charset: string
    val pw_len: int
    val chain_n: int
    val slices: string array
    val chain_length : int
    val max_chains_in_mem: int
    val slave_fifos : (int * string) JoinFifo.t array
    val master_fifo : (int * int) JoinFifo.t
    val result_table : (string option) Util.async_array
    val counter : Util.counter
    val hash_table : string array
    val myid : int
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
        gen_load := !gen_load + 1

    let search_work table = match C.slave_fifos.(myid).get () with
      | None -> (match C.master_fifo.get () with
		 | None -> if C.counter.get () <= 0 then ()
			   else search_work table
		 | Some (hashid, col) ->
		    C.counter.incr ();
		    let hash = C.hash_table.(hashid)
  		    in let last = MyTable.Chain hash col (C.chain_length - col)
		       in for i = 0 to Array.length C.slave_fifos -1 do
			    C.slave_fifos.(i).put (hashid, col, last)
			  done;
			  C.counter.decr ();
			  search_work table)
      | Some (hashid, col, last) ->
	 let firsts = Hashtbl.findall table last
	 in match MyTable.recover C.hash_table.(hashid) col firsts with
	    | None -> C.search_work table
	    | Some pw -> C.result_table.put (i, pw)
				   
end
