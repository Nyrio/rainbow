type 'a t = {content: 'a option; mutable next: 'a cell option}

let is_empty queue = queue.next = None
				    
let create () = {content = None; next = None}
	      
let add tail x =
    let node = {content = Some x; next = None} in
    tail.next <- Some node;
    node

let spawn_queue () =
    def state(tail) & add(x) = state(add tail x) & relpy to add in
    let queue = create () in							  
    spawn state(queue);
    (queue, add)
