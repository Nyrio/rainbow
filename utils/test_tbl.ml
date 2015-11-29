let rec pws_of_seeds rdx_f step state = match step state with
| None -> []
| Some (seed, state) -> rdx_f 0 seed :: pws_of_seeds rdx_f step state

let test table pw = match Table.crack table (table.hash_f pw) with
| None -> print_endline (pw ^ ": not found")
| Some x -> assert (x = pw); print_endline (pw ^ ": found")

let () =
    let in_file = Sys.argv.(1) in
    let table = Table.load in_file in

    let seed_enum = Crypto.rc4seeds (Crypto.get_hash_len table.hash_id) (int_of_string Sys.argv.(2)) in
    let pws = pws_of_seeds table.rdx_f seed_enum.step (seed_enum.start ()) in
    List.iter (test table) pws;
