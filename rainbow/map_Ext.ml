module Make(Ord : Map.OrderedType) = struct
    include Map.Make(Ord)

    type 'a impl =
        | Empty
        | Node of 'a impl * key * 'a * 'a impl * int

    let rec succ_aux node x m = match node with
    | Empty -> m
    | Node(l, k, v, r, _) ->
            let c = Ord.compare k x in
            if c = 0 then Some (k, v)
            else if c < 0 then succ_aux r x m
            else succ_aux l x (Some (k, v))

    (* ugly cast of 'a impl to 'a t because constructors are not in scope *)
    let succ x root = succ_aux (Obj.magic root) x None
end
