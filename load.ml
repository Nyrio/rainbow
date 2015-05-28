let rec add_all_lines in_chan tbl =
    try
      let line = Scanf.fscanf in_chan "%s\n" (fun x -> x) in
      let i = String.index line ';' in
      let last, first = String.sub line 0 i, String.sub line (i+1) (String.length line - i - 1) in
      Hashtbl.add tbl last first;
      add_all_lines in_chan tbl;
    with
    | End_of_file -> ()

let load n file =
    let in_chan = open_in file in
    let tbl = Hashtbl.create n in
    add_all_lines in_chan tbl;
    tbl
