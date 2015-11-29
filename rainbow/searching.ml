type 'a queue = {put: 'a -> unit; get: unit -> 'a}

let buffered_producer () =
    def state(xs, y :: ys) & get(k) = state(xs, ys) & k(y)
    and state(_ :: _ as xs, []) & get(k) = state([], List.rev xs) & get(k)
    and state(_) & kill() = state([], [])
    and state(xs, ys) & put(x) = state(x :: xs, ys) & reply to put
    and state() in
    spawn state([], []);
    {put = put; sig_end = sig_end} {kill = kill; get = get}

let mux cs =
    def put(x, b) = List.iter (fun c -> spawn c.put(x, b)) cs
    and close() = List.iter (fun c -> c.close()) cs and reply to close in
    {put = put; close = close}

let bufferize c =
    let buf = create_queue () in
    def transmit() = c.(buf.get()); transmit() in
    spawn transmit();
    {put = buf.put; close = c.close}


let end_chain_computer hashs tbl prod =
    def get(k) = kont(k) & prod.get(reader)
     or kont(k) & reader(h_id, col) = k(Crypto.full_chain tbl hashs.(hashid) col) in
    {get = get; kill = prod.kill}

let search_
