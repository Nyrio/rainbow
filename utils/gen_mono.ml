open SimpleTable

let rec pws_of_seeds rdx_f step state = match step state with
| None -> []
| Some (seed, state) -> rdx_f 0 seed :: pws_of_seeds rdx_f step state


let () =
    let argv = Sys.argv in
    let header = {chain_len = int_of_string argv.(1);
                  hash_id = argv.(3); charset = argv.(4);
                  pw_len = int_of_string argv.(5)} in
    let chain_num = int_of_string argv.(2) in
    let out_file = argv.(6) in

    let module F = (val SimpleTable.make_fct header) in
    let module T = Table.Make(F) in

    let table = SimpleTable.create header (2*chain_num) in
    let seed_enum = Crypto.random_seeds (Crypto.get_hash_len header.hash_id) chain_num in

    let test table pw = match T.crack table (F.hash_f pw) with
    | None -> print_endline (pw ^ ": not found")
    | Some _ -> print_endline (pw ^ ": found") in


    let rec populate tbl step state = match step state with
    | None -> ()
    | Some (seed, state) -> SimpleTable.add tbl (T.endpoint seed 0) seed; populate tbl step state in

    populate table seed_enum.step (seed_enum.start ());

    let seed_enum = Crypto.random_seeds (Crypto.get_hash_len table.header.hash_id) 100 in
    let pws = pws_of_seeds F.rdx_f seed_enum.step (seed_enum.start ()) in
    List.iter (test (SimpleTable.lookup table)) pws;



    SimpleTable.dump table out_file;
