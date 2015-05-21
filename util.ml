open Cryptokit

let sha256 = hash_string (Hash.sha256 ())
let sha512 = hash_string (Hash.sha512 ())

let mk_rdx charset length =
    let n_char = String.length charset in
    fun salt h ->
        let res = Bytes.create length in
        for i = 0 to length - 1 do
            Bytes.set res i charset.[(int_of_char h.[i] lxor salt) mod n_char];
        done;
        res


module RC4SeedEnum = struct
    let type t = string * int
    let type elt = string
    let type enum = transform * int

    let start (seed, n) = (Cipher.arcfour seed Cipher.encrypt, n)

    let null = String.make 32 '\000'
    let step (arcfour, n) = match n with
    | 0 -> None
    | _ ->
       let () = arcfour#put_string null in
       Some (arcfour#get_string, arcfour)
end
