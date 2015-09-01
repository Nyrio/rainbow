type 'a t =
    | Empty of 'a cell ref
    | Cell of {content: 'a; mutable next: 'a cell}


let add tail x =
    let node = {content = x; next = empty} in
    match tail with
    | Empty -> ()
    | Node n -> n
    node

let spawn_queue () =
    def state(tail) & add(x) = state(add tail x) & relpy to add in
    spawn state(empty);
    
