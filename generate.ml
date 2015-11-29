let main ip port id =
    let () = print_endline "here is the slave" in
    let addr = Unix.ADDR_INET (Unix.inet_addr_of_string ip, port) in
    let master = Join.Ns.there addr in

    let out_file = (string_of_int id) ^ (Join.Ns.lookup master "table_file": string) in
    let hash_id = (Join.Ns.lookup master "hash_id": string) in
    let rdx_id = (Join.Ns.lookup master "rdx_id": string*int) in
    let chain_len = (Join.Ns.lookup master "chain_len": int) in
    let com = (Join.Ns.lookup master "com": Com.t_com) in

    let pool = (Join.Ns.lookup master "gen_reg": (string -> unit) Join.chan) in

    let table = Table.create chain_len hash_id rdx_id 131071 in

    def work(seed) = reply Table.add_chain table seed to work in

    def wait () & instr(Com.Close, resp) =
        resp(Com.Closed, id) & reply Table.dump table out_file to wait in

    spawn com(instr) & pool(work);
    print_endline "slave: all setup";

    wait();
    print_endline "slave: exiting"

let () =
    let ip, port = Scanf.sscanf Sys.argv.(1) "%s@:%i" (fun x y -> x, y) in
    main ip port (int_of_string Sys.argv.(2))
