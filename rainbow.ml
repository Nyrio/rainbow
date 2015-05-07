open Cryptokit;;

class rainbow_table (hash: (string -> string))
                    (rdx: (int -> string -> string))
                    (m: int)
                    (t: int) =
    object (self)
        val hash = hash  (* hashing function *)
        val rdx = rdx  (* reducing function *)
        val table = Hashtbl.create m
        val m = m
        val t = t

        (* return last value of the chain [i;j[ *)
        method private gen_chain seed i j =
            assert ((0 <= i) && (i <= j) && (j <= t));

            if i = j then seed
            else self#gen_chain (rdx i (hash seed)) (i+1) j
        
        (* initialise the table with a seed list *)
        method generate seeds =
            match seeds with
            | [] -> ()
            | seed :: tail ->
                    Hashtbl.add table (self#gen_chain seed 0 t) seed;
                    self#generate tail

        (* try to recover the password at column i of chain starting with
           seed *)
        method private rec recover h seeds i =
            match seeds with
            | [] -> raise Not_found
            | x :: xs ->
                let pw = self#gen_chain x 0 i in
                if hash pw = h then pw
                else self#recover h xs i

        (* crack the hash, starting from column i downwards *)
        method private crack_aux h i =
            assert ((0 <= i) && (i < t));

            let last = self#gen_chain (rdx i h) (i+1) t in
            if Hashtbl.mem table last then
                let seeds = Hashtbl.find_all table last in
                try self#recover h seeds i
                with Not_found -> if i = 0 then raise Not_found
                                  else self#crack_aux h (i-1)
            else
                if i = 0 then raise Not_found
                else self#crack_aux h (i-1)

        (* crack the given hash *)
        method crack h = self#crack_aux h (t - 1)
    end;;

let sha256 = hash_string (Hash.sha256 ());;
let sha512 = hash_string (Hash.sha512 ());;

let mk_rdx charset length =
    let n_char = String.length charset in
    fun salt h ->
        let res = Bytes.create length in
        for i = 0 to length - 1 do
            Bytes.set res i charset.[(int_of_char h.[i] lxor salt) mod n_char];
        done;
        res;;

let random_seeds rdx n =
    let random () = Random.string Random.secure_rng 32 in
    let rec aux n l =
        if n = 0 then l
        else aux (n-1) ((rdx (sha256 (random ()))) :: l)
    in aux n [];;

let arcfour_seeds rdx n seed =
    let arcfour = Cipher.arcfour seed Cipher.Encrypt in
    let get_block () =
        arcfour#put_string (String.make 32 (char_of_int 0));
        arcfour#get_string in
    let rec aux n l = if n = 0 then l else aux (n-1) (rdx (get_block ())::l)
    in aux n [];;

let rdx = mk_rdx "abcdefghijklmnopqrstuvwxyz0123456789" 5;;
let table = new rainbow_table sha256 rdx 2000000 40;;
let () = table#generate (arcfour_seeds (rdx 0) 2000000 "RandomSeedBlaBla!");;
let h = sha256 "da01e";;
let () = print_endline (table#crack h);;
