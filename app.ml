let setup_generate cfg =
    let (_, hash_size) = Hashtbl.find Crypto.hash_funs cfg.hash_fun in
    let pool = JoinPool.Simple.create (Crypto.)

let master_main cfg =
    let ip = Join.Site.get_local_addr () in
    let master = Join.Ns.Site.here in

    Join.Ns.register master "charset" cfg.charset;
    Join.Ns.register master "pw_len" cfg.pw_len;
    Join.Ns.register master "hash_fun" cfg.hash_fun;
    Join.Ns.register master "chain_len" cfg.chain_len;
    Join.Ns.register master "action" cfg.actions;
    Join.Ns.register master "table_file" cfg.table_file;

    let wait =
        if cfg.action = "generate" then
            setup_generate master cfg
        else if cfg.action = "search" then
            failwith "unknown action"
            (*setup_generate cfg*)
        else
            failwith "unknown action" in

    start_slaves ip cfg.slaves cfg.ssh_user cfg.slave_exec cfg.master_port;
    wait ()


let start_slaves ip slaves ssh_user slave_exec master_port =
  let cmd = (Printf.sprintf "ssh %s@%%s %s %s:%s" ssh_ident
                            ssh_user slave_exec ip master_port) in
  let rec aux cmd slaves = match slaves with
    | [] -> ()
    | (ip, cnt) :: xs ->
       for i = 0 to cnt do
           Unix.system (Printf.sprintf cmd ip);
       done; start_slaves cmd xs
  in
  aux cmd slaves


let slave_main ip port =
    let addr = Unix.ADDR_INET (inet_addr_of_string ip, port) in
    let master = Join.Ns.there addr in

    let action = Join.Ns.lookup master "action" in
    let table_file = Join.Ns.lookup master "table_file" in

    let charset = Join.Ns.lookup master "charset" in
    let pw_len = Join.Ns.lookup master "pw_len" in
    let hash_fun = Join.Ns.lookup master "hash_fun" in
    let chain_len = Join.Ns.lookup master "chain_len" in

    module Params = struct
        let rdx_fun = Crypto.make_rdx charset pw_len
        let hash_fun = Hashtbl.get Crypto.hash_funs hash_fun
        let chain_len = chain_len
    end in

    module T = Table.Make(Params) in

    if action = "generate" then
        let table = Hashtbl.create 131071 in
        let pool = Join.Ns.lookup master "gen_reg" in
        let wait = Join.Ns.lookup master "gen_wait" in

        let work seed = Hashtbl.add table (T.full_chain seed) seed in
        spawn pool(work);

        wait ();
        T.dump table_file table;

    else if action = "search" then
        failwith "cannot search"
        (*let table = T.load table_file in
        let results = Join.Ns.lookup master "search_results" in
        let hashs = Join.Ns.lookup master "hashs" in
        let computing_q = Join.Ns.lookup master "computing_q" in
        let searching_q = Join.Ns.lookup master "searching_q" in
        let searching_q_append = Join.Ns.lookup master "searching_q_append" in*)


let cfg = {charset="abcd0123"; pw_len=4; hash_fun="sha256"; chain_len=50;
           action="generate"; table_file="abcd0123_4_sha256.tbl";
           slaves=["127.0.0.1"; "127.0.0.1"]; ssh_user="popeye";
           slave_exec="/home/popeye/projects/tipe/rainbow/bin/app"; master_port=123456;}

let () =
  let argv = Sys.argv in
  if argv.(1) = "master" then
      master_main cfg
  else if argv.(1) = "slave" then
      let ip, port = Scanf.sscanf Sys.argv.(2) "%s:%i" (fun x -> x) (fun x -> x) in
      slave_main ip port
  else
      usage ()
