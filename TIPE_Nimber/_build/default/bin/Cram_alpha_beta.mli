exception Beta_cutoff of ((int * int * int) option * int * int)
exception Alpha_cutoff of ((int * int * int) option * int * int)
val get_under_vertical_pos :
  bool array array -> int -> int -> (int * int * int) option
val get_up_vertical_pos :
  bool array array -> int -> int -> (int * int * int) option
val get_right_horizontal_pos :
  bool array array -> int -> int -> (int * int * int) option
val iter_heur : bool array array -> int * int * int -> int * int * int
val alpha_beta_coup :
  bool array array ->
  int -> int -> int -> int -> (int * int * int) option * int * int
val minmax : bool array array -> int -> (int * int * int) option * int
val random_strat : bool array array -> int * int * int
(*val play_and_print_alpha_beta_vs_random : bool array array -> unit*)
