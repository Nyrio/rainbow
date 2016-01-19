module type OneWayFct = sig
    (** The type of keys. *)
    type key

    (** The type of ciphertexts. *)
    type cipher

    (** The number of reduction functions. *)
    val chain_len : int

    (** A one-way function from keys to ciphers. *)
    val hash_f : key -> cipher

    (** [rdx_f i] is a function from ciphers to keys if i is in
        [0 ... chain_len - 1]. *)
    val rdx_f : int -> cipher -> key
end


(** Output signature of functor {!Table.Make}. *)
module type S = sig
    (** The type of keys. *)
    type key

    (** The type of ciphertexts. *)
    type cipher

    (** [chain h i n] returns the ([i+n])th element of the chain containing
        [h] at index [i]. *)
    val chain : cipher -> int -> int -> cipher

    (** [endpoint h i] returns the endpoint of the chain containing [h] at
        index [i]. *)
    val endpoint : cipher -> int -> cipher

    (** [recover h i starts] returns the first matching key found at index
        [i] of a chain starting with an element of [starts]. *)
    val recover : cipher -> int -> cipher list -> key option

    (** [crack lookup h] returns an element of the preimage of [h] under
        [hash_f] if found. [lookup] is a function which returns a list of
        possible startpoints for a given endpoint. *)
    val crack : (cipher -> cipher list) -> cipher -> key option
end

(** Functor building an implementation of rainbow-table given a one-way
    function (and associated reduction function family). *)
module Make (F: OneWayFct) : S with type key = F.key and
                                    type cipher = F.cipher
