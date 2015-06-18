let rec start_slaves cmd slaves = match slaves with
| [] -> ()
| (ip, cnt) :: xs ->
    for i = 0 to cnt do
        Unix.system (Printf.sprintf cmd ip);
    done; start_slaves cmd xs

let setup_ns master cfg =

    let tbl_conf = {charset=cfg.charset; pw_len=cfg.pw_len;
                    hash_fun=cfg.hash_fun; chain_len=cfg.chain_len} in
    Join.Ns.register master "tbl_conf" tbl_conf;
    Join.Ns.register master "action" cmd;
    Join.Ns.register master "table_file" cfg.filename;

let setup_generate master cfg =
    let (hash_size, _) = Hashtbl.find Table.hash_funs cfg.hash_fun in
    let pool = (JoinPool.Simple.create
        (Table.rc4seeds hash_size cfg.chain_num)
        (fun () () -> ())
        ()) in
    Join.Ns.register master "gen_wait" pool.wait;
    Join.Ns.register master "gen_reg" pool.register;
    pool.wait


let setup_search master hashs =
    let computing_q = JoinFifo.create () in
    def chan(b) = 0 in
    List.iter (fun x -> computing_q.put(x, chan)) hashs;

    let searching_q, searching_add = Util.create_async_queue () in

    let vars = {
        computing_q = JoinFifo.create ();
        searching_q = searching_q;
        searching_add = searching_add;
        counter = Util.create_counter ();
        hashs = hashs;
        results = Util.create_async_array (Array.length hashs) None} in

    Join.Ns.register master "search_vars" vars;


let usage () =
    print_string "Usage: ./master [cmd] conf\n\n";
    print_string "Commands:\n";
    print_string "  generate  Generate the rainbow table.\n";
    print_string "  search    Search the rainbow table for the hashs from stdin.\n";
    Sys.exit 1


let rec hashs_list_of_stdin xs =
    try hash_list_of_stdin (input_line stdin :: xs)
    with End_of_file -> xs


let () =
    let argv = Sys.argv in
    if (Array.length argv <> 3 || (argv.(1) <> "generate" &&
            argv.(1) <> "search")) then
        usage ()
    else
        let cfg = parse_conf argv.(2) in
        let ip = Join.Site.get_local_addr () in
        let master = Join.Ns.Site.here in

        setup_ns master cfg;

        let wait = match argv.(1) with
                   | "generate" -> start_generate master;
                   | "search" -> start_search master (hashs_list_of_stdin []) in

        let cmd = (Printf.sprintf "ssh -i %s %s@%%s %s %s:%s" cfg.ssh_ident
                   cfg.ssh_user cfg.slave_exec ip cfg.master_port) in
        start_slaves cmd cfg.slaves
