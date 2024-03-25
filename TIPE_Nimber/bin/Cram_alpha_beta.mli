val alpha_beta : bool array array -> int -> int -> int -> int -> (int * int * int) option * int * int
val play_and_print_alpha_beta_vs_random : bool array array -> unit
val random_strat : bool array array -> (int * int * int)
val minmax : bool array array -> int -> (int * int * int) option * int