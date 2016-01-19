type rt_header = {chain_len: int; hash_id:string; charset: string;
                  pw_len: int}

type t = {data: (string, string) Hashtbl.t; header: rt_header}

let create header n = {data = Hashtbl.create n; header = header}

let lookup tbl hash = Hashtbl.find_all tbl.data hash

let add tbl endpoint start = Hashtbl.add tbl.data endpoint start

let header_aux in_chan =
    let h = input_line in_chan in
    Scanf.sscanf h "%i\000%s@\000%s@\000%i\000%i\000"
                 (fun a b c d e -> ({chain_len=a;hash_id=b;charset=c;pw_len=d}, e))

let read_header file =
    let chan = open_in file in
    let header, _ = header_aux chan in
    close_in chan;
    header

let load ?tbl file =
    let in_chan = open_in file in
    let h, size = header_aux in_chan in
    let tbl = match tbl with
    | None -> create h size
    | Some tbl ->
        if h <> tbl.header then
            (close_in in_chan;
             invalid_arg "load: header missmatch")
        else tbl in

    let h_len = Crypto.get_hash_len tbl.header.hash_id in
    for i = 0 to size - 1 do
        let last, first = String.create h_len, String.create h_len in
        really_input in_chan last 0 h_len;
            really_input in_chan first 0 h_len;
            add tbl last first;
    done;
    close_in in_chan;
    tbl

let dump tbl file =
    let out_chan = open_out file in
    let h = tbl.header in
    Printf.fprintf out_chan "%i\000%s\000%s\000%i\000%i\000\n"
                   h.chain_len h.hash_id h.charset
                   h.pw_len (Hashtbl.length tbl.data);
    Hashtbl.iter (fun k v -> output_string out_chan (k^v)) tbl.data;
    close_out out_chan

let make_fct header : (module Table.OneWayFct with type key = string and type cipher = string) =
    (module struct
            type key = string
            type cipher = string
            let chain_len = header.chain_len
            let hash_f = Crypto.make_hash_f header.hash_id
            let rdx_f = Crypto.make_rdx_f header.charset header.pw_len
                                          (Crypto.get_hash_len header.hash_id)
     end)
