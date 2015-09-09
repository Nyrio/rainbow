(** Tableau avec acces concurrent. *)
type 'a async_array = {get: int -> 'a; put: (int * 'a) -> unit}

let create_async_array size init =
    def state(a) & get(i) = state(a) & reply a.(i) to get
     or state(a) & put(i, x) = (a.(i) <- x; state(a)) & relpy to put in
    spawn state(Array.create size init);
    {get; put}
