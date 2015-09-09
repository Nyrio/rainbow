type 'a t =
    | End
    | Node of {content: 'a; mutable next: 'a t}


let empty = End


let add tail x =
    let node = {content = x; next = End} in
    tail.next <- node;
    node


let is_empty queue = queue.next = End


let create_async () =
    def state(tail) & append(x) = state(add tail x) & relpy to append in
    let queue = End in
    spawn state(queue);
    (queue, append)
