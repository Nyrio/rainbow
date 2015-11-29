let rec pws_of_seeds rdx_f step state = match step state with
| None -> []
| Some (seed, state) -> rdx_f 0 seed :: pws_of_seeds rdx_f step state

let test table pw = match Table.crack table (table.hash_f pw) with
| None -> print_endline (pw ^ ": not found")
| Some _ -> print_endline (pw ^ ": found")


let rec populate tbl step state = match step state with
| None -> ()
| Some (seed, state) -> Table.add_chain tbl seed; populate tbl step state


let () =
    let argv = Sys.argv in
    let chain_len = int_of_string argv.(1) in
    let chain_num = int_of_string argv.(2) in
    let hash_id = argv.(3) in
    let rdx_id = (argv.(4), int_of_string argv.(5)) in
    let out_file = argv.(6) in

    let table = Table.create chain_len hash_id rdx_id (2*chain_num) in
    let seed_enum = Crypto.rc4seeds (Crypto.get_hash_len hash_id) chain_num in

    populate table seed_enum.step (seed_enum.start ());

    let seed_enum = Crypto.rc4seeds (Crypto.get_hash_len table.hash_id) 100 in
    let pws = pws_of_seeds table.rdx_f seed_enum.step (seed_enum.start ()) in
    List.iter (test table) pws;



    Table.dump table out_file;

