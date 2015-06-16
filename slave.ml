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


    let generation_work (table : (string, string) Hashtbl.t) (seed: string) =
      let last = MyTable.full_chain seed
      in Hashtbl.add table last seed


    let search_start hashs hashs_results counter master_q slaves_q slaves_add table =
        let rec search_work slaves_q =
            let elt, slaves_tl = Util.pop slaves_q in
            match elt with
    	    | None ->
	       (* je n'ai plus de fin de chaine a chercher, il faut demander au master
               la prochaine fin de chaine a calculer *)
	       (match master_q.get () with
  		| None ->
		   (* le master n'a rien a proposer, peut-etre que des slaves sont en
                   train de calculer les dernieres fins, d'ou la verification du
                   compteur *)
	           if counter.get () <= 0 then ()
	           else search_work slaves_tl
	       | Some (hashid, col) ->
	          counter.incr ();
	          let last = T.get_last hashs.(hashid) col in
		  spawn slaves_add (hashid, col, last);
		  counter.decr ();
		  search_work slaves_tl)
	    | Some (hashid, col, last) ->
	        (* verifier si j'ai le hash dans ma table, si oui verifier si ce n'est
                pas une fausse alerte*)
	        let firsts = Hashtbl.findall table last in
	        match T.recover hashs.(hashid) col firsts with
	        | None -> search_work slaves_tl
	        | Some pw ->
		    hashs_results.put (hashid, Some pw);
		    search_work slaves_tl in
	search_work slaves_q
end
