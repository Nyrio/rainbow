open Core.Std

type config = {
  master_port: int;
  slaves: (string*int) list;
  hash_fun: string;
  chain_num: int;
  chain_len: int;
  charset: string;
  ssh_user: string;
  ssh_ident: string;
  slave_exec: string;
}

let filter (json : Yojson.Basic.json)  =
  (json |> member "ip" |> to_string, json |> member "cores" |> to_string)
  
  

let parse_conf (json_filename: string) =
  let json_file = args.(0) in
  let json = Yojson.Basic.from_file json_filename  in
  let open Yojson.Basic.Util in
  { port = json |> member "master_port" |> to_int;
    slaves = json |> member "slaves" |> to_list |> List.map filter;
    hash_fun = json |> member "hash_fun" |> to_string;
    chain_num = json |> member "chain_num" |> to_int;
    chain_len = json |> member "chain_len" |> to_int;
    charset = json |> member "charset" |> to_string;
    ssh_user = json |> member "ssh_user" |> to_string;
    ssh_ident = json |> member "ssh_ident" |> to_string;
    slaver_exec = json |> member "slave_exec" |> to_string }
