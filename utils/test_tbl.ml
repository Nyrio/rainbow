let () =
    let in_file = Sys.argv.(1) in
    let header = SimpleTable.read_header in_file in

    let module F = (val SimpleTable.make_fct header) in
    let module T = Table.Make(F) in

    let table = SimpleTable.load in_file in
    let seed_enum = Crypto.random_seeds (Crypto.get_hash_len header.hash_id) (int_of_string Sys.argv.(2)) in

    let rec pws_of_seeds rdx_f step state = match step state with
    | None -> []
    | Some (seed, state) -> rdx_f 0 seed :: pws_of_seeds rdx_f step state in

    let test table pw = match T.crack table (F.hash_f pw) with
    | None -> print_endline (pw ^ ": not found")
    | Some x -> assert (x = pw); print_endline (pw ^ ": found") in

    let pws = pws_of_seeds (F.rdx_f) seed_enum.step (seed_enum.start ()) in
    List.iter (test (SimpleTable.lookup table)) pws
