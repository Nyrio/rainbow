let files = Array.sub Sys.argv 3 (Array.length Sys.argv - 3)

let table = Table.load Sys.argv.(2)

let () =
    for i = 0 to Array.length files - 1 do
        Table.append table files.(i);
    done

let () = Table.dump table Sys.argv.(1)
