let start ip port =
    let addr = Unix.ADDR_INET (inet_addr_of_string ip, port) in
    let master = Join.Ns.there addr in

    let tbl_conf = Join.Ns.lookup master "tbl_conf": Master.tbl_conf in
    module TblConf = struct
        let rdx_fun = Table.make_rdx tbl_conf.charset tbl_conf.pw_len
        let hash_fun = Hashtbl.get Table.hash_funs tbl_conf.hash_fun
        let chain_len = tbl_conf.chain_len
    end in
    let module S = Make(Table.Make(TblConf)) in

    let action = Join.Ns.lookup master "action": Master.action in
    match action with
    | "generate" -> S.launch_generation master
    | "search" -> S.launch_search master


module Make(T:Table) = struct
    let generation_work table seed =
        let last = T.full_chain seed
        in Hashtbl.add table last seed

    let launch_generation master =
        let table = Hashtbl.create 131071 in
        let pool_reg = Join.Ns.lookup master "gen_reg": (string -> unit) Join.chan in
        let wait = Join.Ns.lookup master "gen_wait": (unit -> unit) in
        let filename = Join.Ns.lookup master "table_file": string in

        spawn pool_reg(generation_work table);
        wait ();
        T.dump filename table;

    let search_work table vars =
        let rec aux searching_q =
            if not Util.Queue.is_empty searching then
                (* verifier si j'ai le hash dans ma table, si oui verifier si
                   ce n'est pas une fausse alerte*)
                let (id, col, last), searching_tl = Util.Queue.pop searching_q in
                let firsts = Hashtbl.findall table last in
                match T.recover vars.hashs.(id) col firsts with
                | None -> aux searching_tl
                | Some pw ->
                    vars.hashs_results.put (id, Some pw);
                    aux searching_tl in
            else
                (* je n'ai plus de fin de chaine a chercher, il faut demander
                   au master la prochaine fin de chaine a calculer *)
                match vars.computing_q.get () with
                | None ->
                    (* le master n'a rien a proposer, peut-etre que des slaves sont
                    en train de calculer les dernieres fins, d'ou la verification du
                    compteur *)
                    if vars.counter.get () <= 0 then ()
                    else Util.sleep 0.5; aux searching_q
                | Some (id, col) ->
                    vars.counter.incr ();
                    let last = T.get_last vars.hashs.(id) col in
                    vars.searching_add (id, col, last);
                    vars.counter.decr ();
                    aux searching_q
        aux

    let launch_search master =
        let vars = Join.Ns.lookup master "search_vars" in
        let filename = Join.Ns.lookup master "table_file" in
        let table = T.load filename in
        search_work table vars vars.searching_q;
end

let () =
    if Array.length Sys.argv <> 2 then
        print_string "Wrong arguments.\nUsage: ./slave master-ip:master-port";
        Sys.exit 1;
    else
        let ip, port = Scanf.sscanf Sys.argv.(1) "%s:%i" (fun x -> x) (fun x -> x) in
        start ip port;
