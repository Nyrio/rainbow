type instruction = Close
type response = Closed

type t_com = (instruction * ((response * int) Join.chan)) Join.chan Join.chan

let chan_mux cs =
    def mux(x) = List.iter (fun c -> spawn c(x)) cs; 0 in
    mux

type 'a queue = {put: 'a Join.chan; get: unit -> 'a option; reg_mon: unit Join.chan Join.chan}

let create_queue init =
    def state(ys, xs) & put(x) = state(ys, x :: xs)
     or state(x :: xs, ys) & get() = state(xs, ys) & reply Some x to get
     or state([], (_ :: _ as ys)) & get() = state(List.rev ys, []) & reply get() to get
     or state([], []) & get() = state([], []) & reply None to get
     or state([], []) & reg_mon(end_chan) = end_chan() & state([], []) in
    spawn state(List.rev init, []);
    {put = put; get = get; reg_mon = reg_mon}

(*
type 'a aarray = {set: (int * 'a) Join.chan; get: int -> 'a}

let create_aarray size init =
    let arr = Array.create size init in
    def lock() & set(i, x) = arr.(i) <- x; lock()
     or lock() & get(i) = lock() & reply arr.(i) to get in
    spawn lock();
    {set = set; get = get}
*)
type ('a, 'b) ahashtbl =
    {add: ('a * 'b) Join.chan; find: 'a -> 'b; mem: 'b -> bool; iter: ('a -> 'b -> unit) -> unit}

let create_ahashtbl size =
    let tbl = Hashtbl.create size in
    def lock() & add(k, v) = Hashtbl.add tbl k v; lock()
     or lock() & find(k) = lock () & reply Hashtbl.find tbl k to find
     or lock() & mem(k) = lock () & reply Hashtbl.mem tbl k to mem
     or lock() & iter(f) = lock () & reply Hashtbl.iter f tbl to iter in
    spawn lock();
    {add = add; find = find; mem = mem; iter = iter}

type counter = {incr: unit -> unit; decr: unit -> unit; get: unit -> int}

let create_counter () =
    def state(n) & incr() = state(succ n) & reply to incr
     or state(n) & decr() = state(pred n) & reply to decr
     or state(n) & get() = state(n) & reply n to get in
    spawn state(0);
    {get = get; incr = incr; decr = decr}
