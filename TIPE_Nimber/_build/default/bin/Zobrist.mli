val hash_table : (int32, int32) Hashtbl.t
val restore : unit -> unit
val save : unit -> unit
val generate_hash_table : int * int -> int32 array array
val store_hash_table : int * int -> int32 array array -> unit
val get_hash_table : int * int -> int32 array array
val verify_hash : string -> unit
