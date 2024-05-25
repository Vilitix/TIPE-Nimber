val hash_table : (int * int * int, int) Hashtbl.t
val extract_key : string -> int * int * string
val restore : unit -> (int * int * int, int) Hashtbl.t
val save : (int * int * int, int) Hashtbl.t -> unit
val generate_hash_table : int * int -> int array array
val store_hash_table : int * int -> int array array -> unit
val get_hash_table : int * int -> int array array
val init_hash : bool array array -> int
