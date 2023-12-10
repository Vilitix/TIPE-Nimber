type union_find 
val init : int -> union_find
val find : union_find -> int -> int
val union : union_find -> int -> int -> unit
val get_tab : union_find -> int array