val hash_table : (int * int * int32, int32) Hashtbl.t
val extract_key : string -> int * int * string
val restore : unit -> unit
val save : unit -> unit
val generate_hash_table : int * int -> int32 array array
val store_hash_table : int * int -> int32 array array -> unit
val get_hash_table : int * int -> int32 array array
val init_hash : bool array array -> int
val save_single : int->int->unit
