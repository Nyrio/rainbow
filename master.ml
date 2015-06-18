open Core.Std

type config = {
  port: int;
  slaves: (string*int) list;
  hash_fun: string;
  chain_num: int;
  chain_len: int;
  charset: string;
}

let parse_conf (json_filename: string) =
  let json_file = args.(0) in
  let json = Yojson.Basic.from_file json_filename  in
  let open Yojson.Basic.Util in
  (
    let port = json |> member "port" |> to_int in
    let slaves = json |> member "slaves" |> to_list |>  in
    let hash_fun = json |> member "hash_fun" |> to_string in
    let chain_num = json |> member "chain_num" |> to_int in
    let chain_len = json |> member "chain_len" |> to_int in
    let charset = json |> member "charset" |> to_string in
  )

https://realworldocaml.org/v1/en/html/handling-json-data.html
