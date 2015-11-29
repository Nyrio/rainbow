let foldl1 f l = match l with
| [] -> failwith "empty list"
| x :: xs -> List.fold_left f x xs

let start_slaves ip slaves ssh_user slave_exec master_port =
  let cmd_part = Scanf.format_from_string (
      Printf.sprintf "(ssh %s@%%s %s %s:%i %%i &)" ssh_user slave_exec ip master_port) "%s%i" in
  let cmd = foldl1 (fun cmd1 cmd2 -> cmd1 ^ ";" ^ cmd2) (List.mapi (fun id ip -> Printf.sprintf cmd_part ip id) slaves) in
  print_endline cmd;
  Unix.system cmd


let main () =
    let ip = Unix.string_of_inet_addr (Join.Site.get_local_addr ()) in
    let master = Join.Ns.here in

    Join.Ns.register master "hash_id" Config.hash_id;
    Join.Ns.register master "rdx_id" Config.rdx_id;
    Join.Ns.register master "chain_len" Config.chain_len;
    Join.Ns.register master "table_file" Config.table_file;

    let com = JoinCount.Collector.create (fun x xs -> x :: xs) [] (List.length Config.slaves) in
    Join.Ns.register master "com" com.collect;

    let exit_collector = JoinCount.Collector.create (fun (_, id) () -> print_endline (Printf.sprintf "slave with id %i is done." id))
                                                    () (List.length Config.slaves) in

    let hash_len = Crypto.get_hash_len Config.hash_id in
    let pool = JoinPool.Simple.create (Crypto.rc4seeds hash_len Config.chain_num)
                                      (fun () () -> ()) () in
    Join.Ns.register master "gen_reg" pool.register;
    Join.Site.listen (Unix.ADDR_INET (Join.Site.get_local_addr(), Config.master_port));


    print_endline "master: starting slaves";
    start_slaves ip Config.slaves Config.ssh_user Config.slave_exec Config.master_port;
    print_endline "master: started slaves";

    let instr = Com.chan_mux (com.wait ()) in
    print_endline "master: received every com chans";

    pool.wait ();
    print_endline "master: everyone is done, sending exit sig";

    spawn instr(Com.Close, exit_collector.collect);
    print_endline "master: exit sent";
    exit_collector.wait ();
    print_endline "everyone exited, exiting"


let () =
    main ()
