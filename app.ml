let master_main cfg =
  let ip = Join.Site.get_local_addr () in
  let master = Join.Ns.Site.here in
  
  Join.Ns.register master "charset" cfg.charset;
  Join.Ns.register master "pw_len" cfg.pw_len;
  Join.Ns.register master "hash_fun" cfg.hash_fun;
  Join.Ns.register master "chain_len" cfg.chain_len;
  Join.Ns.register master "action" cfg.actions;
  Join.Ns.register master "table_file" cfg.table_file;

  let wait = if cfg.action = "generate" then
	       setup_generate cfg
	     else if cfg.action = "search" then
	       setup_generate cfg
	     else
	       failwith "unknown action" in
  
  start_slaves ip cfg.slaves cfg.ssh_ident cfg.ssh_user cfg.slave_exec
	       cfg.master_port cfg.slaves;
  wait ()


let start_slaves ip slaves ssh_ident ssh_user slave_exec master_port =
  let cmd = (Printf.sprintf "ssh -i %s %s@%%s %s %s:%s" ssh_ident
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
    
	   
	   

let () =
  let argv = Sys.argv in
  if (Array.lenght argv <> 3 || (argv.(1) <> "master" && argv.(1) <> "slave")) then
    usage ()
  else if argv.(1) = "master" then
    let cfg = parse_conf argv.(2) in
    master_main cfg
  else
    let ip, port = Scanf.sscanf Sys.argv.(2) "%s:%i" (fun x -> x) (fun x -> x) in
    slave_main ip port
