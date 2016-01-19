(** The type of the rainbow-table files headers. *)
type rt_header = {chain_len: int; hash_id:string; charset: string;
                  pw_len: int}

(** The type of the usual rainbow tables. *)
type t = {data: (string, string) Hashtbl.t; header: rt_header}

(** [create header n] returns a new empty table of size [n] with given
    header. *)
val create : rt_header -> int -> t

(** [lookup tbl endpoint] returns a list of possible starts for the given
    chain endpoint. *)
val lookup : t -> string -> string list

(** [add tbl endpoint start] adds [start] to the possibles startpoints of
    [endpoint]. *)
val add : t -> string -> string -> unit

(** Load a table dump into a table (checking for header match if [tbl]
    given). *)
val load : ?tbl:t -> string -> t

(** Writes the table to the given file. *)
val dump : t -> string -> unit

(** Returns the header of the given rainbow-table file. *)
val read_header : string -> rt_header

(** Creates one-way and reduction functions to be passed to {!Table.Make}. See
    {!Crypto}. *)
val make_fct : rt_header -> (module Table.OneWayFct with type key = string and type cipher = string)
